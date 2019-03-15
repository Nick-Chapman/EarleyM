module OlegExample(tests) where

import Prelude hiding (fail,exp,seq)
import NewLang
import Testing

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
  (s',s) <- declare
  (a',a) <- declare
  (b',b) <- declare
  (c',c) <- declare

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
  run "a"  No,
  run "aa" No,
  run ""  No
  ]
  where
    tag = "oleg"
    run input expect = printCompare tag input actual expect
      where actual = NewLang.parse lang input
