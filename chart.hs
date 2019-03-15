{-#LANGUAGE ExplicitForAll, Rank2Types, ExistentialQuantification #-}

module Chart (NT,withNT,
              Gram,get,alt,alts,fail,
              Production,produce,
              Lang,mkLang,
              Outcome(..),Pos,parse,
              parseEffort)
       where

import Prelude hiding (exp,fail,lex)
import Control.Monad(liftM, ap)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import Data.Map(Map)

import qualified Data.HMap as HMap
import Data.HMap(HKey,HMap)

import qualified Pipe
import Pipe(Pipe)

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f xs = Maybe.catMaybes (List.map f xs)

-- standard instances

instance Functor Gram where fmap = liftM
instance Applicative Gram where pure = unit; (<*>) = ap
instance Monad Gram where (>>=) = bind

-- non primitive combinators

alts :: [Gram a] -> Gram a
alts = foldl alt fail

-- types

data NT a = forall x. NT (HKey x (PipeMap a))

withKeyOfNT :: NT a -> (forall x. HKey x (PipeMap a) -> b) -> b
withKeyOfNT (NT key) k = k key

data Production = forall a. Production (NT a) (Gram a)

type Pos = Int

type Step = (Pos,Pos,Production)

type PipeMap a = Map Pos (Pipe (a,Pos) Step)

type State = HMap

data Gram a = forall b. Read (NT b) (b -> Gram a) 
            | Prod a
            | Done
            | Alt (Gram a) (Gram a)

data Outcome a = Yes a
               | Amb Int [a] 
               | No --Pos -- No useful Pos info yet!
               deriving (Show,Eq)

withNT :: (NT a -> b) -> b
withNT k = HMap.withKey $ \key -> k (NT key)

unit :: a -> Gram a
unit a = Prod a

get :: NT a -> Gram a
get nt = Read nt Prod

alt :: Gram a -> Gram a -> Gram a
alt = Alt

fail :: Gram a
fail = Done

bind :: Gram a -> (a -> Gram b) -> Gram b
bind gram f = case gram of
   Done -> Done
   Alt g1 g2 -> Alt (bind g1 f) (bind g2 f)
   Read nt k -> Read nt (\a -> bind (k a) f)
   Prod a  -> f a

produce :: NT a -> [Gram a] -> Production
produce nt gs = Production nt (alts gs)

data Lang t a = Lang { lex :: NT t,
                       start :: NT a,
                       productions :: [Production] }

mkLang :: NT t -> Gram a -> [Production] -> Lang t a
mkLang lex accept productions =
  withNT $ \start->
  let prodStart = Production start accept in
  Lang lex start (prodStart : productions)

parse :: Lang t a -> [t] -> Outcome a
parse lang input =
  let (_,outcome) = doParse lang input in
  outcome

parseEffort :: Lang t a -> [t] -> Int
parseEffort lang input =
  let (effort,_) = doParse lang input in
  effort

doParse :: Lang t a -> [t] -> (Int,Outcome a)
doParse lang input =
  runString input initPos initState
  where
    initPos = 0
    initState = runProductions 0 (productions lang) HMap.empty
    lenInput = length input
    runString input pos state0 =
      let effort = effortState (productions lang) pos state0  in
      case input of
      [] ->
        let results =
              withKeyOfNT (start lang) $ \key ->
              let pmap = HMap.findWithDefault Map.empty key state0 in
              let xs = Pipe.elems (Map.findWithDefault Pipe.empty initPos pmap) in
              filterMap (\(a,q) -> if q==lenInput then Just a else Nothing) xs in
        let outcome = case results of
              [] -> No --lenInput
              [x] -> Yes x
              xs -> Amb (length xs) xs
        in
        (effort, outcome)
      c : string ->
        let state1 = runSteps [ (pos, pos+1, Production (lex lang) (Prod c)) ] state0 in
        let state2 = runProductions (pos+1) (productions lang) state1 in
        runString string (pos+1) state2

effortState :: [Production] -> Pos -> State -> Int
effortState productions finalPos state = sum $ do
  pos <- [0..finalPos]
  Production nt _ <- productions
  withKeyOfNT nt $ \key ->
    let pmap = HMap.findWithDefault Map.empty key state in
    let pipe = Map.findWithDefault Pipe.empty pos pmap in
    return (length (Pipe.elems pipe))

runProductions :: Pos -> [Production] -> State -> State
runProductions pos prods s = foldr (runProduction pos) s prods

runProduction :: Pos -> Production -> State -> State
runProduction pos prod = runSteps [(pos,pos,prod)]

runSteps :: [Step] -> State -> State
runSteps [] state = state
runSteps ((p1,p2,Production nt gram) : steps) state0 = case gram of
  Done -> runSteps steps state0
  Alt g1 g2 -> runSteps ((p1,p2,Production nt g1):(p1,p2,Production nt g2):steps) state0
  Prod a ->
    let (state,steps') = writeState nt p1 (a,p2) state0 in
    runSteps (steps' ++ steps) state
  Read ntR k ->
    let reader (a,p3) = (p1,p3, Production nt (k a)) in
    let (state,steps') = readState ntR p2 reader state0 in
    runSteps (steps' ++ steps) state

writeState :: NT a -> Pos -> (a,Pos) -> State -> (State,[Step])
writeState nt pos item state0 =
  withKeyOfNT nt $ \key ->
  let pmap0 = HMap.findWithDefault Map.empty key state0 in
  let pipe0 = Map.findWithDefault Pipe.empty pos pmap0 in
  let (pipe,steps) = Pipe.write item pipe0 in
  let pmap = Map.insert pos pipe pmap0 in
  let state = HMap.insert key pmap state0 in
  (state,steps)

readState :: NT a -> Pos -> ((a,Pos) -> Step) -> State -> (State,[Step])
readState nt pos reader state0 =
  withKeyOfNT nt $ \key ->
  let pmap0 = HMap.findWithDefault Map.empty key state0 in
  let pipe0 = Map.findWithDefault Pipe.empty pos pmap0 in
  let (pipe,steps) = Pipe.read reader pipe0 in
  let pmap = Map.insert pos pipe pmap0 in
  let state = HMap.insert key pmap state0 in
  (state,steps)
