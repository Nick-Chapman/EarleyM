{-#LANGUAGE ScopedTypeVariables,ExistentialQuantification,GADTs,RankNTypes,DeriveFunctor#-}

-- | Monadic combinators for Earley Parsing
module EarleyTracing (
  Gram,token,symbol,alts,fail,many,skipWhile, 
  Lang,NT,declare,produce,share,fix, 
  Parsing(..),parse,parseAmb,parseT,parseAmbT,
  SyntaxError(..),Ambiguity(..),ParseError(..),Pos,Eff(..)
  ) where

import Prelude hiding (exp,fail,lex)
import Debug.Trace
import Control.Monad(liftM, ap)
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.HMap as HMap
import Data.HMap(HKey,HMap)
import qualified Pipe
import Pipe(Pipe)

data NtName = NtName String | Start | Token deriving Eq
instance Show NtName where
  show (NtName s) = s
  show Start = "<start>"
  show Token = "<token>"

data NT t a = forall x. NT NtName (a -> String) (HKey x (StateValue t a)) -- TODO: add 2nd key into rules

withNT :: Show a => NtName -> (NT t a -> b) -> b
withNT name k = HMap.withKey $ \key -> k (NT name show key)

withKeyOfNT :: NT t a -> (forall x. HKey x (StateValue t a) -> b) -> b
withKeyOfNT (NT _ _ key) k = k key

instance Show (NT t a) where
  show (NT name _ _) = show name


data Gram t a where
  GetToken :: (t -> Gram t a) -> Gram t a
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
  GetToken k -> GetToken (\t -> k t >>= f)
  Ret a -> f a
  Alts gs -> Alts (do g <- gs; return (g >>= f))
  GetNT nt k -> GetNT nt (\a -> k a >>= f)

token :: Gram t t
token = GetToken Ret

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
createNamedNT name = withNT (NtName name) $ \nt -> Lang nt []

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


type Pos = Int
type From t a = (NT t a, Pos)
type Upto a = (a, Pos)

data Partial
  = forall t a. Predict (From t a)
  | forall t a. Complete (From t a) (Upto a)

instance Show Partial where
  show (Predict (NT name _ _,pos1)) =
    "? " ++ show name ++ "/" ++ show pos1
  show (Complete (NT name aShow _,pos1) (a,pos2)) =
    "! " ++ show name ++ "/" ++ show pos1 ++ " --> " ++ show pos2 ++ " : " ++ aShow a

-- An Earley item: A located/dotted Rule. i.e. Rule + 2 positions
data Item t = forall a. Item Pos (NT t a) Pos (Gram t a)


type Chan t a = Pipe (Upto a) (Item t)

data State = State { chans :: HMap }
type StateValue t a = Map Pos (Chan t a)

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
                  

newtype Eff = Eff Int deriving Show

incEff :: Eff -> Eff
incEff (Eff x) = Eff (x+1)

-- | Result of running a 'parse' function.
-- Combines the 'a' outcome with an indication of the effort taken and the list of partial parse items (for debugging)
data Parsing a = Parsing { effort :: Eff, partials :: [Partial], outcome :: a } deriving Functor


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
parse = gparse False

parseT :: (Show a, Show t) => Lang t (Gram t a) -> [t] -> Parsing (Either ParseError a)
parseT = gparse True

parseAmbT :: (Show a, Show t) => Lang t (Gram t a) -> [t] -> Parsing (Either SyntaxError [a])
parseAmbT = gparseAmb True

parseAmb :: (Show a, Show t) => Lang t (Gram t a) -> [t] -> Parsing (Either SyntaxError [a])
parseAmb = gparseAmb False


gparse :: (Show a, Show t) => Bool -> Lang t (Gram t a) -> [t] -> Parsing (Either ParseError a)
gparse doTrace lang input =
  fmap f (ggparse rejectAmb doTrace lang input)
  where
   f (Left e) = Left e
   f (Right []) = error "gparse, [] results not possible"
   f (Right [x]) = Right x
   f (Right (_:_)) = Left (AmbiguityError (Ambiguity "start" 0 (length input)))
   
gparseAmb :: (Show a, Show t) => Bool -> Lang t (Gram t a) -> [t] -> Parsing (Either SyntaxError [a])
gparseAmb doTrace lang input =
  fmap f (ggparse allowAmb doTrace lang input)
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

data ParseRun t a = ParseRun [t] (Parsing a)

instance (Show t, Show a) => Show (ParseRun t a) where
  show (ParseRun input parsing) =
    unlines [
      "Input: " ++ show input,
      unlines (map show (partials parsing)),
      "Effort = " ++ show (effort parsing),
      "Outcome = " ++ show (outcome parsing)
      ]

ggparse :: (Show a, Show t) => Config -> Bool -> Lang t (Gram t a) -> [t] -> Parsing (Outcome a)
ggparse config doTrace lang input =
  if doTrace
  then traceShow (ParseRun input parsing) parsing
  else parsing
  where
    parsing = 
      withNT Token $ \tokenNT ->
      withNT Start $ \startNT ->
      let Lang gram rules = lang in
      go config tokenNT (startNT,gram) rules input  

go :: Config -> NT t t -> (NT t a, Gram t a) -> [Rule t] -> [t] -> Parsing (Outcome a)
go config tokenNT (startNT,gram) rules input =
  let initState = State { chans = HMap.empty } in
  let state0 = insertChan initState (startNT,0) Pipe.empty in
  let startItem = Item 0 startNT 0 gram in
  let eff0 = Eff 0 in
  case execItemsWithRules config tokenNT rules eff0 [startItem] state0 of
  (eff,partials1,Left ambiguity) -> Parsing eff partials1 (Left (AmbiguityError ambiguity))
  (eff,partials1,Right state1) ->
    let (eff1,partials2,outcome) = loop config tokenNT startNT rules eff 0 state1 input in
    Parsing eff1 (partials1 ++ partials2) outcome

outcomeOfState :: Pos -> Bool -> NT t a -> State -> Outcome a
outcomeOfState pos stillLooking start s = outcome
  where
    outcome = case results of
      [] ->
        if stillLooking
        then Left (SyntaxError (UnexpectedEOF (pos+1)))
        else Left (SyntaxError (UnexpectedTokenAt pos))
      xs -> Right xs
    results = do
      (a,p) <- elems
      if p == pos then [a] else []
    elems =
      case lookChan s (start,0) of
       Just chan -> Pipe.elems chan
       Nothing -> error "startChan missing"

loop :: Config -> NT t t -> NT t a -> [Rule t] -> Eff -> Pos -> State -> [t] -> (Eff,[Partial],Outcome a)
loop _ tokenNT startNT _ eff pos s [] =
  (eff, [], outcomeOfState pos stillLooking startNT s)
  where
    stillLooking = existsChan s (tokenNT,pos)
loop config tokenNT startNT rules eff pos s (x:xs) =
  let from = (tokenNT,pos) in
  case lookChan s from of
   Nothing ->
     if fullParseAt startNT pos s
     then (eff, [], Left (SyntaxError (ExpectedEOF (pos+1))))
     else (eff, [], Left (SyntaxError (UnexpectedTokenAt pos)))
   Just chan ->
     let upto = (x,pos+1) in
     let partials0 = [Complete from upto] in
     let (chan',items) = Pipe.write upto chan in
     let s2 = insertChan s from chan' in
     case execItemsWithRules config tokenNT rules eff items s2 of
      (eff1,partials1,Left ambiguity) -> (eff1,partials1,Left (AmbiguityError ambiguity))
      (eff1,partials1,Right s3) ->
        let (eff2,partials2,outcome) = loop config tokenNT startNT rules eff1 (pos+1) s3 xs in
         (eff2, partials0 ++ partials1 ++ partials2, outcome)


execItemsWithRules :: forall t. Config -> NT t t -> [Rule t] -> Eff -> [Item t] -> State -> (Eff,[Partial],Either Ambiguity (State))
execItemsWithRules config tokenNT rules eff items state = execItems eff items state
  where

    findRules :: NT t a -> [Rule t]
    findRules nt = do
      rule <- rules
      if isRuleKeyedBy nt rule then return rule else []

    execItems :: Eff -> [Item t] -> State -> (Eff,[Partial],Either Ambiguity (State))
    execItems eff [] s = (eff,[],Right s)
    execItems eff (Item p1 nt p2 gram : items) s = case gram of
      Alts gs -> execItems eff ((do g <- gs; return (Item p1 nt p2 g)) ++ items) s

      Ret a ->
        case produceState s from upto of
         Left ambiguity -> (eff,partials,Left ambiguity)
         Right (s1,items1) ->
           push partials (execItems (incEff eff) (items1 ++ items) s1)
        where
          partials = [Complete from upto]
          from = (nt,p1)
          upto = (a,p2)

      GetToken kT -> 
        push (partials1) (execItems (incEff eff) (items1 ++ items) s1)
        where
          (partials1,s1,items1) = awaitState s from reader 
          from = (tokenNT,p2)
          reader (t,p3) = (Item p1 nt p3 nextG)
            where nextG = kT t

      GetNT ntB kB ->
        push (partials1) (execItems (incEff eff) (items1 ++ items) s1)
        where
          (partials1,s1,items1) = awaitState s from reader 
          from = (ntB,p2)
          reader (b,p3) = (Item p1 nt p3 nextG)
            where nextG = kB b

      where
        push ps1 (eff,ps2,s) = (eff,ps1++ps2,s)


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
           Nothing -> Left (Ambiguity (show nt) p1 p2)
             where (nt,p1) = from
                   (_,p2) = upto
           
           Just (chan',items) ->         
             Right (insertChan s from chan', items)

    writeChanNoAmb :: Upto a -> Chan t a -> Maybe (Chan t a, [Item t])
    writeChanNoAmb upto chan =
      if amb then Nothing else Just (Pipe.write upto chan)
      where
        amb = or$ do (_,p) <- Pipe.elems chan; return (p == pos)
        (_,pos) = upto


    awaitState :: State -> From t a -> (Upto a -> Item t) -> ([Partial], State, [Item t])
    awaitState s from reader =
      case lookChan s from of
       Nothing -> (partials, insertChan s from chan, items)
         where chan = Pipe.firstRead reader
               items = predict from
               partials = [Predict from]
       Just chan ->
         ([], insertChan s from chan', items)
         where (chan',items) = Pipe.read reader chan

    predict :: From t a -> [Item t]
    predict (nt,pos) = do
      rule <- findRules nt
      return (itemOfRule pos rule)

    itemOfRule :: Pos -> Rule t -> Item t
    itemOfRule pos (Rule nt gram) = Item pos nt pos gram
