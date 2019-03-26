module EarlyWikiExample(tests) where

import Prelude hiding (fail,exp,seq,lex)
import qualified Data.Char as Char
import Testing
import Earley


(-->) :: NT a -> Gram a -> Lang t ()
(-->) = produce

seq :: [Gram String] -> Gram String
seq gs = do xs <- sequence gs; return ("(" ++ concat xs ++ ")")


tests :: [IO Bool]
tests = [
  do print (mkStaticLang lang); return True,
  run "2+3*4" (Right "(2+(3*4))")
  ]
  where
    tag = "earley-wiki"
    run = check (outcome . parse lang) tag
    lang :: Lang Char (Gram String)
    lang = do
      sat <- satisfy
      sym <- symbol

      let dig = sat"dig" $ \c -> if Char.isDigit c then Just [c] else Nothing
      let lit x = do sym x; return [x]
      
      (s',s) <- declare"S"
      (m',m) <- declare"M"
      (t',t) <- declare"T"
      s' --> seq [s, lit '+', m]
      s' --> m
      m' --> seq [m, lit '*', t]
      m' --> t
      produce t' dig
      return s
