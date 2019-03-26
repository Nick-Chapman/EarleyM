{-#LANGUAGE Rank2Types, ExistentialQuantification, DeriveFunctor, GADTs #-}

module Earley(NT,Gram,alts,fail,many,skipWhile,
              Lang, satisfy,token,symbol, declare,produce,share,fix, 
              StaticLang,mkStaticLang,

              Pos, SyntaxError(..), Ambiguity(..), ParseError(..), Eff(..), Parsing(..),
              parse,  parseAmb, parseT, parseAmbT
             )
where

import Prelude hiding (exp,fail,lex)
import Debug.Trace
import Control.Monad(liftM, ap)
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.HMap as HMap
import Data.HMap(HKey,HMap)
import Data.List
import qualified Pipe
import Pipe(Pipe)

data NtName = NtName String | Start | Terminal deriving Eq
instance Show NtName where
  show (NtName s) = s
  show Start = "<start>"
  show Terminal = "<terminal>"

data NT a = forall x. NT NtName (a -> String) (HKey x (StateValue a))

nameOfNt :: NT a -> NtName
nameOfNt (NT name _ _) = name

withNT :: Show a => NtName -> (NT a -> b) -> b
withNT name k = HMap.withKey $ \key -> k (NT name show key)

withKeyOfNT :: NT a -> (forall x. HKey x (StateValue a) -> b) -> b
withKeyOfNT (NT _ _ key) k = k key

instance Show (NT a) where
  show (NT name _ _) = show name


data Gram a where
  Ret :: a -> Gram a
  Alts :: [Gram a] -> Gram a
  Sat :: String -> (NT b) -> (b -> Maybe c) -> (c -> Gram a) -> Gram a
  Many :: Gram a -> Gram [a]
  Bind :: Gram a -> (a -> Gram b) -> Gram b
         
instance Functor Gram where fmap = liftM
instance Applicative Gram where pure = return; (<*>) = ap

instance Monad Gram where
  return = Ret
  (>>=) = Bind

bind :: Gram a -> (a -> Gram b) -> Gram b
bind gram f = case gram of
  Ret a -> f a
  Alts gs -> Alts (do g <- gs; return (g >>= f))
  Sat s nt p k -> Sat s nt p (\a -> k a >>= f)
  Many g -> Alts [f [], do x <- g; xs <- Many g; f (x : xs)]
  Bind g f1 -> Bind g (\x -> f1 x >>= f)

satNT :: NT a -> String -> (a -> Maybe b) -> Gram b
satNT nt s p = Sat s nt p Ret

referenceNT :: NT a -> Gram a
referenceNT nt = satNT nt (show nt) Just

alts :: [Gram a] -> Gram a
alts = Alts

fail :: Gram a
fail = Alts []

many :: Gram a -> Gram [a]
many = Many

skipWhile :: Gram () -> Gram ()
skipWhile p = do _ <- many p; return ()


data Rule = forall a. Rule (NT a) (Gram a)

isRuleKeyedBy :: NT a -> Rule -> Bool
isRuleKeyedBy nt1 (Rule nt2 _) =
  withKeyOfNT nt1 $ \key1 ->
  withKeyOfNT nt2 $ \key2 ->
  HMap.unique key1 == HMap.unique key2


type Satisfy t = forall a. (String -> (t -> Maybe a) -> Gram a)
data Lang t a = Lang { runLang :: Satisfy t -> (a, [Rule]) }

instance Functor (Lang t) where fmap = liftM
instance Applicative (Lang t) where pure = return; (<*>) = ap

instance Monad (Lang t) where
  return a = Lang$ \_ -> (a,[])
  (>>=) m f = Lang$ \t ->
    let (a,ps1) = runLang m t in
    let (b,ps2) = runLang (f a) t in
    (b,ps1++ps2)

satisfy :: Lang t (String -> (t -> Maybe a) -> Gram a)
satisfy = Lang$ \sat -> (sat,[])

token :: Lang t (Gram t)
token = do
  sat <- satisfy
  return (sat "." Just)

symbol :: (Show t, Eq t) => Lang t (t -> Gram ())
symbol = do
  sat <- satisfy
  return$ \x -> sat (show x) (\t -> if t==x then Just () else Nothing)

createNamedNT :: Show a => String -> Lang t (NT a)
createNamedNT name = Lang$ \_ -> withNT (NtName name) $ \nt -> (nt,[])

declare :: Show a => String -> Lang t (NT a, Gram a)
declare name = do
  nt <- createNamedNT name
  return (nt, referenceNT nt)

produce :: NT a -> Gram a -> Lang t ()
produce nt gram = Lang$ \_ -> ((),[Rule nt gram])

share :: Show a => String -> Gram a -> Lang t (Gram a)
share name gram = do
  (nt,g2) <- declare name
  produce nt gram
  return g2
  
fix :: Show a => String -> (Gram a -> Lang t (Gram a)) -> Lang t (Gram a)
fix name f = do
  (nt,gram) <- declare name
  fixed <- f gram
  () <- produce nt fixed
  return gram


type Pos = Int
type From a = (NT a, Pos)
type Upto a = (a, Pos)

data Partial
  = forall a. Predict (From a)
  | forall a b. Dot (From a) (NT b)
  | forall a. Complete (From a) (Upto a)

instance Show Partial where
  show (Predict (NT name _ _,pos1)) =
    "? " ++ show name ++ "/" ++ show pos1
  show (Dot (NT name _ _,pos1) (NT name2 _ _)) =
    "? " ++ show name ++ "/" ++ show pos1 ++ " --> ?? " ++ show name2 ++ " ??"
  show (Complete (NT name aShow _,pos1) (a,pos2)) =
    "! " ++ show name ++ "/" ++ show pos1 ++ " --> " ++ show pos2 ++ " : " ++ aShow a


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

writeChanNoAmb :: Upto a -> Chan a -> Maybe (Chan a, [Item])
writeChanNoAmb upto chan =
  if amb then Nothing else Just (Pipe.write upto chan)
  where
    amb = or$ do (_,p) <- Pipe.elems chan; return (p == pos)
    (_,pos) = upto

data State = State { chans :: HMap }
type StateValue a = Map Pos (Chan a)

existsChan :: State -> From a -> Bool
existsChan s (nt,pos) =
  withKeyOfNT nt $ \key ->
  case HMap.lookup key (chans s) of
   Nothing -> False
   Just m -> Map.member pos m

lookChan :: State -> From a -> Maybe (Chan a)
lookChan s (nt,pos) =
  withKeyOfNT nt $ \key ->
  case HMap.lookup key (chans s) of
   Nothing -> Nothing
   Just m -> Map.lookup pos m

insertChan :: State -> From a -> Chan a -> State
insertChan s (nt,pos) chan =
  withKeyOfNT nt $ \key ->
  let m = HMap.findWithDefault Map.empty key (chans s) in
  let m' = Map.insert pos chan m in
  s { chans = HMap.insert key m' (chans s) }

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
                  

newtype Eff = Eff Int deriving Show

incEff :: Eff -> Eff
incEff (Eff x) = Eff (x+1)

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


parse :: (Show a, Show t) => Lang t (Gram a) -> [t] -> Parsing (Either ParseError a)
parse = gparse False

parseT :: (Show a, Show t) => Lang t (Gram a) -> [t] -> Parsing (Either ParseError a)
parseT = gparse True

parseAmbT :: (Show a, Show t) => Lang t (Gram a) -> [t] -> Parsing (Either SyntaxError [a])
parseAmbT = gparseAmb True

parseAmb :: (Show a, Show t) => Lang t (Gram a) -> [t] -> Parsing (Either SyntaxError [a])
parseAmb = gparseAmb False


gparse :: (Show a, Show t) => Bool -> Lang t (Gram a) -> [t] -> Parsing (Either ParseError a)
gparse doTrace lang input =
  fmap f (ggparse rejectAmb doTrace lang input)
  where
   f (Left e) = Left e
   f (Right []) = error "gparse, [] results not possible"
   f (Right [x]) = Right x
   f (Right (_:_)) = Left (AmbiguityError (Ambiguity "start" 0 (length input)))
   
gparseAmb :: (Show a, Show t) => Bool -> Lang t (Gram a) -> [t] -> Parsing (Either SyntaxError [a])
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

ggparse :: (Show a, Show t) => Config -> Bool -> Lang t (Gram a) -> [t] -> Parsing (Outcome a)
ggparse config doTrace lang input =
  if doTrace
  then traceShow (ParseRun input parsing) parsing
  else parsing
  where
    parsing = 
      withNT Terminal $ \tokenNT ->
      withNT Start $ \startNT ->
      let (gram,rules) = runLang lang (satNT tokenNT) in
      go config tokenNT (startNT,gram) rules input  

go :: Config -> NT t -> (NT a, Gram a) -> [Rule] -> [t] -> Parsing (Outcome a)
go config token (start,gram) rules input =
  let initState = State { chans = HMap.empty } in
  let state0 = insertChan initState (start,0) Pipe.empty in
  let startItem = Item 0 start 0 gram in
  let eff0 = Eff 0 in
  case execItemsWithRules config rules eff0 [startItem] state0 of
  (eff,partials1,Left ambiguity) -> Parsing eff partials1 (Left (AmbiguityError ambiguity))
  (eff,partials1,Right state1) ->
    let (eff1,partials2,outcome) = loop config token start rules eff 0 state1 input in
    Parsing eff1 (partials1 ++ partials2) outcome

outcomeOfState :: Pos -> Bool -> NT a -> State -> Outcome a
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

loop :: Config -> NT t -> NT a -> [Rule] -> Eff -> Pos -> State -> [t] -> (Eff,[Partial],Outcome a)
loop _ token start _ eff pos s [] =
  (eff, [], outcomeOfState pos stillLooking start s)
  where
    stillLooking = existsChan s (token,pos)
loop config token start rules eff pos s (x:xs) =
  let from = (token,pos) in
  case lookChan s from of
   Nothing ->
     if fullParseAt start pos s
     then (eff, [], Left (SyntaxError (ExpectedEOF (pos+1))))
     else (eff, [], Left (SyntaxError (UnexpectedTokenAt pos)))
   Just chan ->
     let upto = (x,pos+1) in
     let partial = Complete from upto in -- TODO: dont bother to add partials for tokens?
     let (chan',items) = writeChan upto chan in
     let s2 = insertChan s from chan' in
     case execItemsWithRules config rules eff items s2 of
      (eff1,partials1,Left ambiguity) -> (eff1,partials1,Left (AmbiguityError ambiguity))
      (eff1,partials1,Right s3) ->
        let (eff2,partials2,outcome) = loop config token start rules eff1 (pos+1) s3 xs in
         (eff2, partial : partials1 ++ partials2, outcome)


execItemsWithRules :: Config -> [Rule] -> Eff -> [Item] -> State -> (Eff,[Partial],Either Ambiguity (State))
execItemsWithRules config rules eff items state = execItems eff items state
  where

    findRules :: NT a -> [Rule]
    findRules nt = do
      rule <- rules
      if isRuleKeyedBy nt rule then return rule else []

    execItems :: Eff -> [Item] -> State -> (Eff,[Partial],Either Ambiguity (State))
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

      Sat _ ntB satB fC ->
        push (partials0 ++ partials1) (execItems (incEff eff) (items1 ++ items) s1)
        where
          partials0 = [] --Dot (nt,p1) ntB]
          (partials1,s1,items1) = awaitState s from reader 
          from = (ntB,p2)
          reader (b,p3) = (Item p1 nt p3 nextG)
            where nextG = case satB b of
                    Just c -> fC c
                    Nothing -> Alts []

      Many g -> execItems eff (Item p1 nt p2 many_g : items) s
        where 
          many_g = alts [return [], do x <- g; xs <- many_g; return (x : xs)]

      Bind g f -> execItems eff (Item p1 nt p2 bind_g_f : items) s
        where 
          bind_g_f = bind g f

      where
        push ps1 (eff,ps2,s) = (eff,ps1++ps2,s)


    produceState :: State -> From a -> Upto a -> Either Ambiguity (State, [Item])
    produceState s from upto =
      case lookChan s from of
       Nothing -> error ("produceState, missing channel for: " ++ show from)
       Just chan ->
         if allowAmbiguity config
         then
           case writeChan upto chan of
           (chan',items) ->         
             Right (insertChan s from chan', items)
         else
           case writeChanNoAmb upto chan of
           Nothing -> Left (Ambiguity (show nt) p1 p2)
             where (nt,p1) = from
                   (_,p2) = upto
           
           Just (chan',items) ->         
             Right (insertChan s from chan', items)

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


----------------------------------------------------------------------

data StaticElem
  = Elem String
  | Star [StaticElem]
    
instance Show StaticElem where
  show (Elem s) = s
  show (Star [e]) = show e ++ "*"
  show (Star es) = "(" ++ intercalate " " (map show es) ++ ")*"

type StaticRhs = [StaticElem]
data StaticRule = StaticRule NtName StaticRhs
instance Show StaticRule where
  show (StaticRule lhs rhs) =
    intercalate " " ([show lhs, "-->"] ++ map show rhs)

data StaticLang = StaticLang NtName [StaticRule]
instance Show StaticLang where
  show (StaticLang start rules) =
    intercalate "\n" ("" : (show start ++ " where") : map show rules) ++ "\n"
                  
mkStaticOfGram :: [StaticElem] -> Gram a -> [StaticRhs]
mkStaticOfGram elems gram = case gram of
  Ret _  -> [elems]
  Alts gs -> concat (map (mkStaticOfGram elems) gs)
  Sat s _ _ k -> mkStaticOfGram (Elem s : elems) (k undef)
  Many g -> do
    es <- mkStaticOfGram [] g
    [Star (reverse es) : elems]
  Bind g f -> do
    es <- mkStaticOfGram elems g
    mkStaticOfGram es (f undef)
  where
    undef = error "mkStatic, use of undefined"

mkStaticOfRule :: Rule -> [StaticRule]
mkStaticOfRule (Rule nt gram) = do
  es <- mkStaticOfGram [] gram
  [StaticRule (nameOfNt nt) (reverse es)]

mkStaticLang :: (Show t, Show a) => Lang t (Gram a) -> StaticLang
mkStaticLang lang =
  withNT Terminal $ \tokenNT ->
  withNT Start $ \startNT ->
  let (gram,rules) = runLang lang (satNT tokenNT) in
  let rules1 = Rule startNT gram : rules in
  StaticLang (nameOfNt startNT) (concat (map mkStaticOfRule rules1))
