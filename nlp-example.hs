module NlpExample(test) where

-- Prepositional phrase attachment ambiguity example. See:
-- https://allthingslinguistic.com/post/52411342274/how-many-meanings-can-you-get-for-the-sentence-i

import Prelude hiding (fail,exp,seq,lex)
import Testing
import Chart
import Data.List

data Tree = Word String | Phrase [Tree] deriving (Eq)

instance Show Tree where
  show (Word s) = s
  show (Phrase ts) = "(" ++ intercalate " " (map show ts) ++ ")"

lang :: Lang String (Gram Tree)
lang = do
  tok <- token
  let lex ws = alts (map (\w -> do
                             w' <- tok
                             if w==w' then return () else fail
                             return (Word w)
                         ) ws)

  let pro  = lex ["I"]
  let det  = lex ["the","a"]
  let verb = lex ["saw"]
  let noun = lex ["man","telescope","hill"]    
  let prep = lex ["on","with"]    
  
  (s',s)   <- declare"S"
  (np',np) <- declare"NP"
  (vp',vp) <- declare"VP"
  (pp',pp) <- declare"PP"

  np' --> seq [pro]
  np' --> seq [det,noun]
  vp' --> seq [verb,np]
  vp' --> seq [verb,np,pp]
  pp' --> seq [prep,np]
  np' --> seq [np,pp]
  s'  --> seq [np,vp]
  
  return s


seq :: [Gram Tree] -> Gram Tree
seq = fmap Phrase . sequence

(-->) :: NT a -> Gram a -> Lang t ()
(-->) = produce


test :: IO Bool
test =
  run "I saw the man on the hill with a telescope"
  [
    "((I) (saw ((the man) (on ((the hill) (with (a telescope)))))))",
    "((I) (saw (the man) (on ((the hill) (with (a telescope))))))",
    "((I) (saw (((the man) (on (the hill))) (with (a telescope)))))",
    "((I) (saw ((the man) (on (the hill))) (with (a telescope))))"
  ]
  where
    tag = "telescope"
    run input xs = runW (Prelude.words input) (Multiple (length xs) xs)
    (_,runW) = runTestParseThen allowAmb (\(Parsing _ _ o) -> fmap show o) tag lang
