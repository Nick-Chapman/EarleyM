module Example.Arith(tests) where

import Prelude hiding (seq,fail)
import qualified Data.Char as Char
import Testing
import Earley


(-->) :: NT a -> Gram a -> Lang t ()
(-->) = produce

seq :: [Gram String] -> Gram String
seq gs = do xs <- sequence gs; return ("(" ++ concat xs ++ ")")


tests :: [IO Bool]
tests = [
  run "2+3*4" (Right "(2+(3*4))")
  ]
  where
    tag = "earley-wiki"
    run = check (outcome . parse lang) tag
    lang = do

      token <- getToken
      let symbol x = do t <-token; if t==x then return () else fail
      let dig = do c <- token; if Char.isDigit c then return [c] else fail
      let lit x = do symbol x; return [x]
      
      (s',s) <- declare"S"
      (m',m) <- declare"M"
      (t',t) <- declare"T"
      s' --> seq [s, lit '+', m]
      s' --> m
      m' --> seq [m, lit '*', t]
      m' --> t
      produce t' dig
      return s
