{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- | Monadic combinators for Earley Parsing
module EarleyM (
  -- * Grammars, Non-terminals, Productions
  Gram, alts, fail, many, skipWhile,
  Lang, getToken,
  NT, createNamedNT, referenceNT, declare, produce, fix,

  -- * Parsing
  Parsing(..), SyntaxError(..), parseAmb,
  Ambiguity(..), ParseError(..), parse,

  Eff(..), Pos
  ) where

import           Control.Monad (ap, liftM)
import           Data.HMap     (HKey, HMap)
import qualified Data.HMap     as HMap
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           EarleyM.Pipe  (Pipe)
import qualified EarleyM.Pipe  as Pipe
import           Prelude       hiding (exp, fail, lex)

-- | Type of grammars. RHS of a production rules. Synthesizing values of type 'a'. Monadic construction allows context-sensitive grammars.
data Gram a where
  Ret :: a -> Gram a
  Alts :: [Gram a] -> Gram a
  GetNT :: (NT b) -> (b -> Gram a) -> Gram a

instance Functor Gram where fmap = liftM
instance Applicative Gram where pure = return; (<*>) = ap

instance Monad Gram where
  return = Ret
  (>>=) = bind

bind :: Gram a -> (a -> Gram b) -> Gram b
bind gram f = case gram of
  Ret a      -> f a
  Alts gs    -> Alts (do g <- gs; return (g >>= f))
  GetNT nt k -> GetNT nt (\a -> k a >>= f)

-- | Grammar constructed from a list of alteratives. Alternations may be nested; we are not restricted to just having alternate productions for a given non-terminal.
alts :: [Gram a] -> Gram a
alts = Alts

-- | Grammar constructed from no alteratives. @fail == alts []@
fail :: Gram a
fail = Alts []

-- | Grammar for kleene '*'. Constructs an infinite RHS. Use in preference to right-recursion via a non-terminal, which has quadratic inefficiency.
many :: Gram a -> Gram [a]
many g = many_g
  where many_g = alts [return [], do x <- g; xs <- many_g; return (x : xs)]

-- | Kleene '*' which ignores the synthesized value.
--  @ skipWhile p == do _ <- many p; return () @
skipWhile :: Gram () -> Gram ()
skipWhile p = do _ <- many p; return ()


-- | Type of non-terminals. LHS of a production rules. Carrying values of type 'a'.
data NT a = forall x. NT String (HKey x (StateValue a)) -- TODO: add 2nd key into rules

withNT :: String -> (NT a -> b) -> b
withNT name k = HMap.withKey $ \key -> k (NT name key)

withKeyOfNT :: NT a -> (forall x. HKey x (StateValue a) -> b) -> b
withKeyOfNT (NT _ key) k = k key

instance Show (NT a) where
  show (NT name _) = name


data Rule = forall a. Rule (NT a) (Gram a)

isRuleKeyedBy :: NT a -> Rule -> Bool
isRuleKeyedBy nt1 (Rule nt2 _) =
  withKeyOfNT nt1 $ \key1 ->
  withKeyOfNT nt2 $ \key2 ->
  HMap.unique key1 == HMap.unique key2


-- | Type for language definition over terminals (tokens) of type 't'. A collection of production rules together with an entry point. Constructed monadically.
data Lang t a = Lang { runLang :: NT t -> (a, [Rule]) }

instance Functor (Lang t) where fmap = liftM
instance Applicative (Lang t) where pure = return; (<*>) = ap

instance Monad (Lang t) where
  return a = Lang$ \_ -> (a, [])
  (>>=) m f = Lang$ \tok ->
    let (a,rules1) = runLang m tok in
    let (b,rules2) = runLang (f a) tok in
    (b, rules1 ++ rules2)


-- | Access to the grammar for tokens within a language definition.
getToken :: Lang t (Gram t)
getToken = Lang$ \tok -> (referenceNT tok, [])

-- | Create a fresh non-terminal within a language definition. The name is only used for debugging and reporting ambiguity. This is a low level primitive. Simpler to use 'declare'.
createNamedNT :: Show a => String -> Lang t (NT a)
createNamedNT name = withNT name $ \nt -> Lang$ \_ -> (nt, [])

-- | Reference a non-terminal on the RHS of a production. This is a low level primitive. Simpler to use 'declare'.
referenceNT :: NT a -> Gram a
referenceNT nt = GetNT nt Ret

-- | Convenience combination of 'createNamedNT' and 'referenceNT', returning a pair of values for a fresh non-terminal, for use on the LHS/RHS.
declare :: Show a => String -> Lang t (NT a, Gram a)
declare name = do
  nt <- createNamedNT name
  return (nt, referenceNT nt)

-- | Define a language production, linking the LHS and RHS of the rule.
produce :: NT a -> Gram a -> Lang t ()
produce nt gram = Lang$ \_ -> ((), [Rule nt gram])

-- | Combination of declare/produce to allow reference to a grammar within its own defintion. Use this for language with left-recursion.
fix :: Show a => String -> (Gram a -> Lang t (Gram a)) -> Lang t (Gram a)
fix name f = do
  (nt,gram) <- declare name
  fixed <- f gram
  () <- produce nt fixed
  return gram


-- | Type to represent the effort taken during parsing. Used by some unit-tests.
newtype Eff = Eff Int deriving Show

incEff :: Eff -> Eff
incEff (Eff x) = Eff (x+1)

-- | Type of positions. Used in parse error reports. Indicates the index of the input token list reached before the error was encountered.
type Pos = Int


type From a = (NT a, Pos)
type Upto a = (a, Pos)

-- An Earley item: A located/dotted Rule. i.e. Rule + 2 positions
data Item = forall a. Item Pos (NT a) Pos (Gram a)

type Chan a = Pipe (Upto a) (Item)
data State = State { chans :: HMap, effortState :: Eff }
type StateValue a = Map Pos (Chan a)

incEffortState :: State -> State
incEffortState s = s { effortState = incEff (effortState s) }

existsChan :: State -> From a -> Bool
existsChan s (nt,pos) =
  withKeyOfNT nt $ \key ->
  case HMap.lookup key (chans s) of
   Nothing -> False
   Just m  -> Map.member pos m

lookChan :: State -> From a -> Maybe (Chan a)
lookChan s (nt,pos) =
  withKeyOfNT nt $ \key ->
  case HMap.lookup key (chans s) of
   Nothing -> Nothing
   Just m  -> Map.lookup pos m

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
       Nothing   -> error "startChan missing"


-- | Result of running a parsing function: 'parse' or 'parseAmb'. Combines the outcome with the effort taken.
data Parsing a = Parsing { effort :: Eff, outcome :: a } deriving Functor


-- | Type describing a syntax-error encountered during parsing. In all cases the final position reached before the error is reported. This position is automatically determined by the Early parsing algorithm.
data SyntaxError
  = UnexpectedTokenAt Pos
  | UnexpectedEOF Pos
  | ExpectedEOF Pos
  deriving (Show,Eq)

-- | Type describing a parse ambiguity for a specific non-terminal (name), across a position range. This may be reported as an error by the 'parse' entry point.
data Ambiguity = Ambiguity String Pos Pos deriving (Show,Eq)

-- | Union of 'SyntaxError' and 'Ambiguity', for reporting errors from 'parse'.
data ParseError
  = SyntaxError SyntaxError
  | AmbiguityError Ambiguity
  deriving (Show,Eq)


-- | Entry-point to run a parse. Rejects ambiguity. Parse a list of tokens using a Lang/Gram definition. Returns the single parse or a parse-error.
parse :: (Show a, Show t) => Lang t (Gram a) -> [t] -> Parsing (Either ParseError a)
parse lang input =
  fmap f (ggparse rejectAmb lang input)
  where
   f (Left e) = Left e
   f (Right []) = error "gparse, [] results not possible"
   f (Right [x]) = Right x
   f (Right (_:_)) = Left (AmbiguityError (Ambiguity "start" 0 (length input)))


-- | Entry-point to run a parse. Allows ambiguity. Parses a list of tokens using a Lang/Gram definition. Returns all parses or a syntax-error.
parseAmb :: (Show a, Show t) => Lang t (Gram a) -> [t] -> Parsing (Either SyntaxError [a])
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

ggparse :: (Show a, Show t) => Config -> Lang t (Gram a) -> [t] -> Parsing (Outcome a)
ggparse config lang input =
  parsing
  where
    parsing =
      withNT "<token>" $ \tokenNT ->
      let (gram,rules) = runLang lang tokenNT in
      go tokenNT config gram rules input

go :: NT t -> Config -> Gram a -> [Rule] -> [t] -> Parsing (Outcome a)
go tokenNT config gram rules input =
  withNT "<start>" $ \startNT ->
  let initState = State { chans = HMap.empty, effortState = Eff 0 } in
  let state0 = insertChan initState (startNT,0) Pipe.empty in
  let startItem = Item 0 startNT 0 gram in
  let (state1,optAmb) = execItemsWithRules config rules [startItem] state0 in
  case optAmb of
  Just ambiguity ->
    Parsing (effortState state1) (Left (AmbiguityError ambiguity))

  Nothing ->
    let (s,outcome) = loop startNT tokenNT config rules 0 state1 input in
    Parsing (effortState s) outcome

loop :: NT a -> NT t -> Config -> [Rule] -> Pos -> State -> [t] -> (State,Outcome a)
loop startNT tokenNT config rules pos s xs = case xs of
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
       Nothing        -> loop startNT tokenNT config rules (pos+1) s3 xs


execItemsWithRules :: Config -> [Rule] -> [Item] -> State -> (State, Maybe Ambiguity)
execItemsWithRules config rules items state = execItems items state
  where

    findRules :: NT a -> [Rule]
    findRules nt = do
      rule <- rules
      if isRuleKeyedBy nt rule then return rule else []

    execItems :: [Item] -> State -> (State, Maybe Ambiguity)
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

    produceState :: State -> From a -> Upto a -> Either Ambiguity (State, [Item])
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

    writeChanNoAmb :: Upto a -> Chan a -> Maybe (Chan a, [Item])
    writeChanNoAmb upto chan = do
      let (_,pos) = upto
      let amb = or$ do (_,p) <- Pipe.elems chan; return (p == pos)
      if amb then Nothing else Just (Pipe.write upto chan)

    awaitState :: State -> From a -> (Upto a -> Item) -> (State, [Item])
    awaitState s from reader =
      case lookChan s from of
       Nothing -> do
         let chan = Pipe.firstRead reader
         let items = predict from
         (insertChan s from chan, items)
       Just chan -> do
         let (chan',items) = Pipe.read reader chan
         (insertChan s from chan', items)

    predict :: From a -> [Item]
    predict (nt,pos) = do
      rule <- findRules nt
      return (itemOfRule pos rule)

    itemOfRule :: Pos -> Rule -> Item
    itemOfRule pos (Rule nt gram) = Item pos nt pos gram
