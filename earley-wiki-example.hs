module EarlyWikiExample(tests) where

import Prelude hiding (fail,exp,seq,lex)
import qualified Data.Char as Char
import Testing
import Chart


(-->) :: NT a -> Gram a -> Lang t ()
(-->) = produce

seq :: [Gram String] -> Gram String
seq gs = do xs <- sequence gs; return ("(" ++ concat xs ++ ")")

tests :: [IO Bool]
tests = [
  run "2+3*4" (Yes "(2+(3*4))"),
  run "" (No 1)
  ]
  where
    tag = "earley-wiki"
    (run,_run) = runTest tag lang 
    lang :: Lang Char (Gram String)
    lang = do
      tok <- token
      let dig = do c <- tok; if Char.isDigit c then return [c] else fail
      let sym x = do c <- tok; if x==c then return [c] else fail

      (s',s) <- declare"S"
      (m',m) <- declare"M"
      (t',t) <- declare"T"
      s' --> seq [s, sym '+', m]
      s' --> m
      m' --> seq [m, sym '*', t]
      m' --> t
      produce t' dig
      return s
