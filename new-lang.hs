module NewLang(NT,Gram,alts,fail,Outcome(..), --re-exports
               Lang,token,declare,produce,fix,parse) where

import Prelude hiding(fail)
import Control.Monad(liftM, ap)

import Chart(Gram,alts,fail,Outcome)
import Chart(Production,NT)
import qualified Chart

-- standard instances

instance Functor (Lang t) where fmap = liftM
instance Applicative (Lang t) where pure = unit; (<*>) = ap
instance Monad (Lang t) where (>>=) = bind

-- non primitive 

declare :: Lang t (NT a, Gram a)
declare = do
  nt <- createNT
  return (nt, referenceNT nt)

fix :: (Gram a -> Lang t (Gram a)) -> Lang t (Gram a)
fix f = do
  (nt,gram) <- declare
  fixed <- f gram
  () <- produce nt fixed
  return fixed

-- reader/output monad for constructing languages
--    reader for accessing the grammar of tokens
--    output for productions
data Lang t a = Lang { runLang :: Gram t -> (a, [Production]) }

unit :: a -> Lang t a
unit a = Lang$ \_ -> (a,[])
  
bind :: Lang t a -> (a -> Lang t b) -> Lang t b
bind m f = Lang$ \t ->
  let (a,ps1) = runLang m t in
  let (b,ps2) = runLang (f a) t in
  (b,ps1++ps2)

produce :: NT a -> Gram a -> Lang t ()
produce nt gram = Lang$ \_ -> ((),[Chart.produce nt [gram]])

token :: Lang t (Gram t)
token = Lang$ \t -> (t,[])

createNT :: Lang t (NT a)
createNT = Lang$ \_ -> Chart.withNT $ \nt -> (nt,[])
--createNT = Chart.withNT $ \nt -> Lang$ \_ -> (nt,[]) -- core dump!
-- because every nt is the same, and so we get loopy/stack-overflow behaviour ???

referenceNT :: NT a -> Gram a
referenceNT = Chart.get

parse :: Lang t (Gram a) -> [t] -> Outcome a
parse lang input =
  Chart.withNT $ f
  where
    f tok = outcome
      where
        (gram,productions) = runLang lang (referenceNT tok)
        underlying_lang = Chart.mkLang tok gram productions
        outcome = Chart.parse underlying_lang input
