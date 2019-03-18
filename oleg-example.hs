module OlegExample(tests) where

import Prelude hiding (fail,exp,seq)
import Testing
import Chart

(-->) :: NT a -> Gram a -> Lang t ()
(-->) = produce

seq :: [Gram String] -> Gram String
seq gs = do xs <- sequence gs; return ("(" ++ concat xs ++ ")")

-- found example online of pathalogical grammar

{-
https://mail.haskell.org/pipermail/haskell-cafe/2013-February/106512.html
...
	http://okmij.org/ftp/Haskell/LeftRecursion.hs

It can handle eps-cycles, ambiguity and other pathology. Here is a
sample bad grammar:

   S -> S A C | C
   A -> B | aCa
   B -> B
   C -> b | C A
-}

lang :: Lang Char (Gram String)
lang = do
  tok <- token
  let term x = do c <- tok; if c == x then return [c] else fail
  (s',s) <- declare"s"
  (a',a) <- declare"a"
  (b',b) <- declare"b"
  (c',c) <- declare"c"

  s' --> alts [seq [s, a, c], c]
  a' --> alts [b, seq [term 'a', c, term 'a']]
  b' --> b
  c' --> alts [term 'b', seq [c, a]]

  return s

tests :: [IO Bool]
tests = [
  run "b"       (Yes "b"),
  run "baba"    (Yes "(b(aba))"),
  run "bababaa" (Yes "(b(a(b(aba))a))"),
  run "babaaba" (Yes "((b(aba))(aba))"),
  run "babab"   (Yes "(b(aba)b)"),
  run "a"  (No 1),
  run "aa" (No 1),
  run ""  (No 1)
  ]
  where
    tag = "oleg"
    (run,_runX) = runTest tag lang
