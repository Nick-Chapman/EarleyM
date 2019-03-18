{-#LANGUAGE Rank2Types, ExistentialQuantification #-}

module Chart (NT,Gram,alts,fail,
              Lang,token,declare,produce,fix,
              Partial,Outcome(..),Pos,doParse)
where

import Prelude hiding (exp,fail,lex)
import Control.Monad(liftM, ap)
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.HMap as HMap
import Data.HMap(HKey,HMap)
import qualified Pipe
import Pipe(Pipe)


data NT a = forall x. NT String (a -> String) (HKey x (StateValue a))

withNT :: Show a => String -> (NT a -> b) -> b
withNT name k = HMap.withKey $ \key -> k (NT name show key)

withKeyOfNT :: NT a -> (forall x. HKey x (StateValue a) -> b) -> b
withKeyOfNT (NT _ _ key) k = k key

instance Show (NT a) where
  show (NT name _ _) = name


data Gram a = Alts [Gram a] | Ret a | forall b. Get (NT b) (b -> Gram a)

instance Functor Gram where fmap = liftM
instance Applicative Gram where pure = return; (<*>) = ap

instance Monad Gram where
  return = Ret
  (>>=) gram f = case gram of
    Alts gs -> Alts (do g <- gs; return (g >>= f))
    Get nt k -> Get nt (\a -> k a >>= f)
    Ret a  -> f a

referenceNT :: NT a -> Gram a
referenceNT nt = Get nt Ret

alts :: [Gram a] -> Gram a
alts = Alts

fail :: Gram a
fail = Alts []


data Rule = forall a. Rule (NT a) (Gram a)

isRuleKeyedBy :: NT a -> Rule -> Bool
isRuleKeyedBy nt1 (Rule nt2 _) =
  withKeyOfNT nt1 $ \key1 ->
  withKeyOfNT nt2 $ \key2 ->
  HMap.unique key1 == HMap.unique key2


data Lang t a = Lang { runLang :: Gram t -> (a, [Rule]) }

instance Functor (Lang t) where fmap = liftM
instance Applicative (Lang t) where pure = return; (<*>) = ap

instance Monad (Lang t) where
  return a = Lang$ \_ -> (a,[])
  (>>=) m f = Lang$ \t ->
    let (a,ps1) = runLang m t in
    let (b,ps2) = runLang (f a) t in
    (b,ps1++ps2)

token :: Lang t (Gram t)
token = Lang$ \t -> (t,[])

createNamedNT :: Show a => String -> Lang t (NT a)
createNamedNT name = Lang$ \_ -> withNT name $ \nt -> (nt,[])

produce :: NT a -> Gram a -> Lang t ()
produce nt gram = Lang$ \_ -> ((),[Rule nt gram])

declare :: Show a => String -> Lang t (NT a, Gram a)
declare name = do
  nt <- createNamedNT name
  return (nt, referenceNT nt)
  
fix :: Show a => String -> (Gram a -> Lang t (Gram a)) -> Lang t (Gram a)
fix name f = do
  (nt,gram) <- declare name
  fixed <- f gram
  () <- produce nt fixed
  return fixed


type Pos = Int
type From a = (NT a, Pos)
type Upto a = (a, Pos)

data Partial
  = forall a. Predict (From a)
  | forall a. Complete (From a) (Upto a)

instance Show Partial where
  show (Predict (NT name _ _,pos1)) =
    "? " ++ name ++ "/" ++ show pos1
  show (Complete (NT name aShow _,pos1) (a,pos2)) =
    "! " ++ name ++ "/" ++ show pos1 ++ " --> " ++ show pos2 ++ " : " ++ aShow a


data Item -- An Earley item: A located/dotted Rule. i.e. Rule + 2 positions
  = forall a. Item Pos (NT a) Pos (Gram a)

itemOfRule :: Pos -> Rule -> Item
itemOfRule pos (Rule nt gram) = Item pos nt pos gram


type Chan a = Pipe (Upto a) Item -- Is this abstraction worthwhile?

createChanFromReader :: (Upto a -> Item) -> Chan a
createChanFromReader = Pipe.firstRead

readChan :: (Upto a -> Item) -> Chan a -> (Chan a, [Item])
readChan = Pipe.read

writeChan :: Upto a -> Chan a -> (Chan a, [Item])
writeChan = Pipe.write


type State = HMap
type StateValue a = Map Pos (Chan a)

existsChan :: State -> From a -> Bool
existsChan s (nt,pos) =
  withKeyOfNT nt $ \key ->
  case HMap.lookup key s of
   Nothing -> False
   Just m -> Map.member pos m

lookChan :: State -> From a -> Maybe (Chan a)
lookChan s (nt,pos) =
  withKeyOfNT nt $ \key ->
  case HMap.lookup key s of
   Nothing -> Nothing
   Just m -> Map.lookup pos m

insertChan :: State -> From a -> Chan a -> State
insertChan s (nt,pos) chan =
  withKeyOfNT nt $ \key ->
  let m = HMap.findWithDefault Map.empty key s in
  let m' = Map.insert pos chan m in
  HMap.insert key m' s

fullParseAt :: NT a -> Pos -> State -> Bool
fullParseAt start pos s = not (null results)
  where
    results = do
      (a,p) <- elems
      if p == pos then [a] else []
    elems =
      case lookChan s (start,0) of
       Just chan -> Pipe.elems chan
       Nothing -> error "startChan missing"
                  

data Outcome a = No Pos | Yes a | Amb Int [a] deriving (Show,Eq)

outcomeOfState :: Pos -> Bool -> NT a -> State -> Outcome a
outcomeOfState pos stillLooking start s = outcome
  where
    outcome = case results of
      [] ->
        if stillLooking
        then No (pos+1) -- run out of tokens
        else No pos -- final token broke the parse
      [x] -> Yes x
      xs -> Amb (length xs) xs
    results = do
      (a,p) <- elems
      if p == pos then [a] else []
    elems =
      case lookChan s (start,0) of
       Just chan -> Pipe.elems chan
       Nothing -> error "startChan missing"

doParse :: (Show t, Show a) => Lang t (Gram a) -> [t] -> ([Partial],Outcome a)
doParse lang input =
  withNT "T" $ \token ->
  withNT "S" $ \start ->
  let (gram,rules) = runLang lang (referenceNT token) in
  go token (start,gram) rules input  

go :: NT t -> (NT a, Gram a) -> [Rule] -> [t] -> ([Partial], Outcome a)
go token (start,gram) rules input =
  let state0 = insertChan HMap.empty (start,0) Pipe.empty in
  let startItem = Item 0 start 0 gram in
  let (partials1,state1) = execItemsWithRules rules [startItem] state0 in
  let (partials2,outcome) = loop 0 state1 input in
  (partials1 ++ partials2, outcome)
  where
    loop pos s [] =
      ([], outcomeOfState pos stillLooking start s)
      where
        stillLooking = existsChan s (token,pos)
    loop pos s (x:xs) =
      let from = (token,pos) in
      case lookChan s from of
       Nothing ->
         if fullParseAt start pos s
         then ([], No (pos+1)) -- previous token completed the parse, but doesn't want more
         else ([], No pos) -- previous token broke the parse
       Just chan ->
         let upto = (x,pos+1) in
         let partial = Complete from upto in -- TODO: dont bother to add partials for tokens?
         let (chan',items) = writeChan upto chan in
         let s2 = insertChan s from chan' in
         let (partials1,s3) = execItemsWithRules rules items s2 in
         let (partials2,outcome) = loop (pos+1) s3 xs in
         (partial : partials1 ++ partials2, outcome)

execItemsWithRules :: [Rule] -> [Item] -> State -> ([Partial],State)
execItemsWithRules rules items state = execItems items state
  where
    findRules :: NT a -> [Rule]
    findRules nt = do
      rule <- rules
      if isRuleKeyedBy nt rule then return rule else []

    execItems :: [Item] -> State -> ([Partial],State)
    execItems [] s = ([],s)
    execItems (Item p1 nt p2 gram : items) s = case gram of
      Alts gs -> execItems ((do g <- gs; return (Item p1 nt p2 g)) ++ items) s
      Ret a ->
        push partials (execItems (items1 ++ items) s1)
        where
          partials = [Complete from upto]
          (s1,items1) = produceState s from upto
          from = (nt,p1)
          upto = (a,p2)
      Get ntB fB ->
        push partials (execItems (items1 ++ items) s1)
        where
          (partials,s1,items1) = awaitState s from reader 
          from = (ntB,p2)
          reader (b,p3) = (Item p1 nt p3 (fB b))
      where
        push ps1 (ps2,s) = (ps1++ps2,s)

    produceState :: State -> From a -> Upto a -> (State, [Item])
    produceState s from upto =
      case lookChan s from of
       Nothing -> error ("produceState, missing channel for: " ++ show from)
       Just chan -> (insertChan s from chan', items)
         where (chan',items) = writeChan upto chan

    awaitState :: State -> From a -> (Upto a -> Item) -> ([Partial], State, [Item])
    awaitState s from reader =
      case lookChan s from of
       Nothing -> (partials, insertChan s from chan, items)
         where chan = createChanFromReader reader
               items = predict from
               partials = [Predict from]
       Just chan ->
         ([], insertChan s from chan', items)
         where (chan',items) = readChan reader chan

    predict :: From a -> [Item]
    predict (nt,pos) = do
      rule <- findRules nt
      return (itemOfRule pos rule)
