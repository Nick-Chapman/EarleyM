module NewLangExample(tests) where

import Prelude hiding (fail,exp)
import qualified Data.Char as Char

import NewLang
import Testing

digitOfChar :: Char -> Int
digitOfChar c = Char.ord c - ord0 where ord0 = Char.ord '0'

digitTok :: Gram Char -> Gram Int
digitTok tok =
  do c <- tok; if Char.isDigit c then return (digitOfChar c) else fail

symTok :: Eq t => Gram t -> t -> Gram ()
symTok tok x = do c <- tok; if c == x then return () else fail

data E = Leaf Int | Add E E deriving (Show,Eq)
    
lang :: Lang Char (Gram E)
lang = do
  tok <- token
  let digit = digitTok tok
  let sym = symTok tok
  num <- fix $ \num -> return $ alts [
    digit,
    do n <- num; d <- digit; return (10 * n + d)
    ]
  exp <- fix $ \exp -> return $ alts [
    do d <- num; return (Leaf d),
    do e1 <- exp; sym '+' ; e2 <- exp; return (Add e1 e2),
    do sym '(' ; e <- exp; sym ')'; return e
    ]
  return exp

tests :: [IO Bool]
tests = [
  run "5" (Yes (Leaf 5)),
  run "(5)" (Yes (Leaf 5)),
  run "((5))" (Yes (Leaf 5)),
  run "5+6" (Yes (Add (Leaf 5) (Leaf 6))),
  run "5+6+7" (Amb 2 [Add (Add (Leaf 5) (Leaf 6)) (Leaf 7),Add (Leaf 5) (Add (Leaf 6) (Leaf 7))]),
  run "(5+6)+7" (Yes (Add (Add (Leaf 5) (Leaf 6)) (Leaf 7))),
  run "5+(6+7)" (Yes (Add (Leaf 5) (Add (Leaf 6) (Leaf 7)))),
  run "5" (Yes (Leaf 5)),
  run "51" (Yes (Leaf 51)),
  run "(5)" (Yes (Leaf 5)),
  run "(51)" (Yes (Leaf 51)),
  run "51+61" (Yes (Add (Leaf 51) (Leaf 61))),
  run "51+61+71" (Amb 2 [Add (Add (Leaf 51) (Leaf 61)) (Leaf 71),Add (Leaf 51) (Add (Leaf 61) (Leaf 71))]),
  run "(51+61)+71" (Yes (Add (Add (Leaf 51) (Leaf 61)) (Leaf 71))),
  run "51+(61+71)" (Yes (Add (Leaf 51) (Add (Leaf 61) (Leaf 71))))
  ]
  where
    tag = "new-lang-example"
    run input expect =
      let actual = NewLang.parse lang input in
      printCompare tag input actual expect
