module Example.PrepositionalAttachmentAmbiguity(tests) where

-- Prepositional phrase attachment ambiguity example. See:
-- https://allthingslinguistic.com/post/52411342274/how-many-meanings-can-you-get-for-the-sentence-i

import Prelude hiding (fail,exp,seq,lex)
import Testing
import Earley
import Data.List

data Tree = Word String | Phrase [Tree] deriving (Eq)

instance Show Tree where
  show (Word s) = s
  show (Phrase ts) = "(" ++ intercalate " " (map show ts) ++ ")"

lang :: Lang String (Gram Tree)
lang = do

  pro  <- lex"PRO" ["I"]
  det  <- lex"D" ["the","a"]
  verb <- lex"V" ["saw"]
  noun <- lex"N" ["man","telescope","hill"]    
  prep <- lex"P" ["on","with"]    
  
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

lex :: String -> [String] -> Lang String (Gram Tree)
lex name ws = do
  sat <- satisfy
  share name $
    alts (map (\w -> do
                  sat (show w) (\w' -> if w==w' then Just () else Nothing)
                  return (Word w)
              ) ws)

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
    run :: String -> [String] -> IO Bool
    run str xs =
      check f tag input (Right xs)
      where
        input  = Prelude.words str
        f = fmap (map show) . outcome . parseAmb lang



tests :: [IO Bool]
tests =  [
  do print (mkStaticLang lang); return True,
  test
  ]
