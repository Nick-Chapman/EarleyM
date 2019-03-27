{-#LANGUAGE ScopedTypeVariables,ExistentialQuantification,GADTs,RankNTypes,DeriveFunctor#-}

-- | Monadic combinators for Earley Parsing
module Earley (
  Gram,token,symbol,alts,fail,many,skipWhile, 
  Lang,NT,declare,produce,share,fix, 
  Parsing(..),parse,parseAmb,
  SyntaxError(..),Ambiguity(..),ParseError(..),Pos,Eff(..)
  ) where

import Prelude hiding (exp,fail,lex)
import Control.Monad(liftM, ap)
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.HMap as HMap
import Data.HMap(HKey,HMap)
import qualified Pipe
import Pipe(Pipe)

data NT t a = forall x. NT String (HKey x (StateValue t a)) -- TODO: add 2nd key into rules

withNT :: String -> (NT t a -> b) -> b
withNT name k = HMap.withKey $ \key -> k (NT name key)

createNT :: String -> NT t a
createNT name = withNT name id

tokenNT :: NT t a
tokenNT = createNT "<token>"

startNT :: NT t a
startNT = createNT "<start>"

withKeyOfNT :: NT t a -> (forall x. HKey x (StateValue t a) -> b) -> b
withKeyOfNT (NT _ key) k = k key

instance Show (NT t a) where
  show (NT name _) = name

data Gram t a where
  Ret :: a -> Gram t a
  Alts :: [Gram t a] -> Gram t a
  GetNT :: (NT t b) -> (b -> Gram t a) -> Gram t a
         
instance Functor (Gram t) where fmap = liftM
instance Applicative (Gram t) where pure = return; (<*>) = ap

instance Monad (Gram t) where
  return = Ret
  (>>=) = bind

bind :: Gram t a -> (a -> Gram t b) -> Gram t b
bind gram f = case gram of
  Ret a -> f a
  Alts gs -> Alts (do g <- gs; return (g >>= f))
  GetNT nt k -> GetNT nt (\a -> k a >>= f)

--token :: Gram t a -- How can this type be allowed? (it is!)
token :: Gram t t
token = GetNT tokenNT Ret

symbol :: Eq t => t -> Gram t ()
symbol x = do t <-token; if t==x then return () else fail

referenceNT :: NT t a -> Gram t a
referenceNT nt = GetNT nt Ret

alts :: [Gram t a] -> Gram t a
alts = Alts

fail :: Gram t a
fail = Alts []

many :: Gram t a -> Gram t [a] 
many g = many_g
  where many_g = alts [return [], do x <- g; xs <- many_g; return (x : xs)]

skipWhile :: Gram t () -> Gram t ()
skipWhile p = do _ <- many p; return ()

data Rule t = forall a. Rule (NT t a) (Gram t a)

isRuleKeyedBy :: NT t a -> Rule t -> Bool
isRuleKeyedBy nt1 (Rule nt2 _) =
  withKeyOfNT nt1 $ \key1 ->
  withKeyOfNT nt2 $ \key2 ->
  HMap.unique key1 == HMap.unique key2

data Lang t a = Lang a [Rule t]

instance Functor (Lang t) where fmap = liftM
instance Applicative (Lang t) where pure = return; (<*>) = ap

instance Monad (Lang t) where
  return a = Lang a []
  (>>=) (Lang a rules1) f =
    let Lang b rules2 = f a in
    Lang b (rules1 ++ rules2)

createNamedNT :: Show a => String -> Lang t (NT t a)
createNamedNT name = withNT name $ \nt -> Lang nt []

declare :: Show a => String -> Lang t (NT t a, Gram t a)
declare name = do
  nt <- createNamedNT name
  return (nt, referenceNT nt)

produce :: NT t a -> Gram t a -> Lang t ()
produce nt gram = Lang () [Rule nt gram]

share :: Show a => String -> Gram t a -> Lang t (Gram t a)
share name gram = do
  (nt,g2) <- declare name
  produce nt gram
  return g2
  
fix :: Show a => String -> (Gram t a -> Lang t (Gram t a)) -> Lang t (Gram t a)
fix name f = do
  (nt,gram) <- declare name
  fixed <- f gram
  () <- produce nt fixed
  return gram

newtype Eff = Eff Int deriving Show

incEff :: Eff -> Eff
incEff (Eff x) = Eff (x+1)

type Pos = Int
type From t a = (NT t a, Pos)
type Upto a = (a, Pos)

-- An Earley item: A located/dotted Rule. i.e. Rule + 2 positions
data Item t = forall a. Item Pos (NT t a) Pos (Gram t a)

type Chan t a = Pipe (Upto a) (Item t)
data State = State { chans :: HMap, effortState :: Eff }
type StateValue t a = Map Pos (Chan t a)

incEffortState :: State -> State
incEffortState s = s { effortState = incEff (effortState s) }

existsChan :: State -> From t a -> Bool
existsChan s (nt,pos) =
  withKeyOfNT nt $ \key ->
  case HMap.lookup key (chans s) of
   Nothing -> False
   Just m -> Map.member pos m

lookChan :: State -> From t a -> Maybe (Chan t a)
lookChan s (nt,pos) =
  withKeyOfNT nt $ \key ->
  case HMap.lookup key (chans s) of
   Nothing -> Nothing
   Just m -> Map.lookup pos m

insertChan :: State -> From t a -> Chan t a -> State
insertChan s (nt,pos) chan =
  withKeyOfNT nt $ \key ->
  let m = HMap.findWithDefault Map.empty key (chans s) in
  let m' = Map.insert pos chan m in
  s { chans = HMap.insert key m' (chans s) }

fullParseAt :: NT t a -> Pos -> State -> Bool
fullParseAt start pos s = not (null results)
  where
    results = do
      (a,p) <- elems
      if p == pos then [a] else []
    elems =
      case lookChan s (start,0) of
       Just chan -> Pipe.elems chan
       Nothing -> error "startChan missing"

-- | Result of running a 'parse' function.
-- Combines the outcome with an indication of the effort taken
data Parsing a = Parsing { effort :: Eff, outcome :: a } deriving Functor

data SyntaxError
  = UnexpectedTokenAt Pos
  | UnexpectedEOF Pos
  | ExpectedEOF Pos
  deriving (Show,Eq)

data Ambiguity = Ambiguity String Pos Pos deriving (Show,Eq)

data ParseError
  = SyntaxError SyntaxError
  | AmbiguityError Ambiguity
  deriving (Show,Eq)

parse :: (Show a, Show t) => Lang t (Gram t a) -> [t] -> Parsing (Either ParseError a)
parse lang input =
  fmap f (ggparse rejectAmb lang input)
  where
   f (Left e) = Left e
   f (Right []) = error "gparse, [] results not possible"
   f (Right [x]) = Right x
   f (Right (_:_)) = Left (AmbiguityError (Ambiguity "start" 0 (length input)))
   
parseAmb :: (Show a, Show t) => Lang t (Gram t a) -> [t] -> Parsing (Either SyntaxError [a])
parseAmb lang input =
  fmap f (ggparse allowAmb lang input)
  where
   f (Left (SyntaxError e)) = Left e
   f (Left (AmbiguityError _)) = error "gparseAmb, AmbiguityError not possible"
   f (Right xs) = Right xs

data Config = Config { allowAmbiguity :: Bool }

allowAmb :: Config
allowAmb = Config { allowAmbiguity = True }

rejectAmb :: Config
rejectAmb = Config { allowAmbiguity = False }

type Outcome a = Either ParseError [a]

ggparse :: (Show a, Show t) => Config -> Lang t (Gram t a) -> [t] -> Parsing (Outcome a)
ggparse config lang input =
  parsing
  where
    parsing = 
      let Lang gram rules = lang in
      go config gram rules input  

go :: Config -> Gram t a -> [Rule t] -> [t] -> Parsing (Outcome a)
go config gram rules input =
  let initState = State { chans = HMap.empty, effortState = Eff 0 } in
  let state0 = insertChan initState (startNT,0) Pipe.empty in
  let startItem = Item 0 startNT 0 gram in
  let (state1,optAmb) = execItemsWithRules config rules [startItem] state0 in
  case optAmb of
  Just ambiguity ->
    Parsing (effortState state1) (Left (AmbiguityError ambiguity))

  Nothing ->
    let (s,outcome) = loop config rules 0 state1 input in
    Parsing (effortState s) outcome

loop :: Config -> [Rule t] -> Pos -> State -> [t] -> (State,Outcome a)
loop config rules pos s xs = case xs of
  [] ->
    case lookChan s (startNT,0) of
     Nothing -> error "startChan missing"
     Just chan -> do
       let results = do (a,p) <- Pipe.elems chan; if p == pos then [a] else []
       case results of
        [] ->
          let stillLooking = existsChan s (tokenNT,pos) in
          if stillLooking
          then (s, Left (SyntaxError (UnexpectedEOF (pos+1))))
          else (s, Left (SyntaxError (UnexpectedTokenAt pos)))
        xs ->
          (s, Right xs)
        
  x:xs ->
    let from = (tokenNT,pos) in
    case lookChan s from of
    Nothing ->
      if fullParseAt startNT pos s
      then (s, Left (SyntaxError (ExpectedEOF (pos+1))))
      else (s, Left (SyntaxError (UnexpectedTokenAt pos)))
    Just chan ->
      let upto = (x,pos+1) in
      let (chan',items) = Pipe.write upto chan in
      let s2 = insertChan s from chan' in
      let (s3,optAmb) = execItemsWithRules config rules items s2 in
      case optAmb of
       Just ambiguity -> (s, Left (AmbiguityError ambiguity))
       Nothing -> loop config rules (pos+1) s3 xs


execItemsWithRules :: forall t. Config -> [Rule t] -> [Item t] -> State -> (State, Maybe Ambiguity)
execItemsWithRules config rules items state = execItems items state
  where

    findRules :: NT t a -> [Rule t]
    findRules nt = do
      rule <- rules
      if isRuleKeyedBy nt rule then return rule else []

    execItems :: [Item t] -> State -> (State, Maybe Ambiguity)
    execItems [] s = (s, Nothing)
    execItems (Item p1 nt p2 gram : items) s = case gram of
      Alts gs -> do
        let items1 = do g <- gs; return (Item p1 nt p2 g)
        execItems (items1 ++ items) s

      Ret a -> do
        let from = (nt,p1)
        let upto = (a,p2)
        case produceState s from upto of
         Left ambiguity -> (s, Just ambiguity)
         Right (s1,items1) ->
           execItems (items1 ++ items) (incEffortState s1)

      GetNT ntB kB -> do
        let from = (ntB,p2)
        let reader (b,p3) = Item p1 nt p3 (kB b)
        let (s1,items1) = awaitState s from reader
        execItems  (items1 ++ items) (incEffortState s1)

    produceState :: State -> From t a -> Upto a -> Either Ambiguity (State, [Item t])
    produceState s from upto =
      case lookChan s from of
       Nothing -> error ("produceState, missing channel for: " ++ show from)
       Just chan ->
         if allowAmbiguity config
         then
           case Pipe.write upto chan of
           (chan',items) ->         
             Right (insertChan s from chan', items)
         else
           case writeChanNoAmb upto chan of
           Nothing -> do
             let (nt,p1) = from
             let (_,p2) = upto
             Left (Ambiguity (show nt) p1 p2)
           
           Just (chan',items) ->         
             Right (insertChan s from chan', items)

    writeChanNoAmb :: Upto a -> Chan t a -> Maybe (Chan t a, [Item t])
    writeChanNoAmb upto chan = do
      let (_,pos) = upto
      let amb = or$ do (_,p) <- Pipe.elems chan; return (p == pos)
      if amb then Nothing else Just (Pipe.write upto chan)

    awaitState :: State -> From t a -> (Upto a -> Item t) -> (State, [Item t])
    awaitState s from reader =
      case lookChan s from of
       Nothing -> do
         let chan = Pipe.firstRead reader
         let items = predict from
         (insertChan s from chan, items)               
       Just chan -> do
         let (chan',items) = Pipe.read reader chan
         (insertChan s from chan', items)

    predict :: From t a -> [Item t]
    predict (nt,pos) = do
      rule <- findRules nt
      return (itemOfRule pos rule)

    itemOfRule :: Pos -> Rule t -> Item t
    itemOfRule pos (Rule nt gram) = Item pos nt pos gram
