module BasicExamples(tests) where

import Prelude hiding (fail,exp)
import qualified Data.Char as Char
import Testing
import Chart


digitOfChar :: Char -> Int
digitOfChar c = Char.ord c - ord0 where ord0 = Char.ord '0'

digitTok :: Gram Char -> Gram Int
digitTok tok =
  do c <- tok; if Char.isDigit c then return (digitOfChar c) else fail

symTok :: Eq t => Gram t -> t -> Gram ()
symTok tok x = do c <- tok; if c == x then return () else fail

countOutcomes :: [Partial] -> Outcome a -> Int
countOutcomes _ o = case o of
  No _ -> 0
  Yes _ -> 1
  Amb n _ -> n
  AmbError _ -> 0

measureEffort :: [Partial] -> Outcome a -> Int
measureEffort partials _ = length partials

                           
tests1 :: [IO Bool]
tests1 = [
  run "" (No 1),
  run "x" (No 1),
  run "xy" (No 1),
  run "5" (No 2),
  run "5x" (No 2),
  run "5xy" (No 2),
  run "56" (Yes 56),
  run "56x" (No 3),
  run "56xy" (No 3)
  ]
  where
    tag = "2dig"
    (run,_runX) = runTest tag lang 
    lang = do
      tok <- token
      let digit = digitTok tok
      return $ do
        d1 <- digit
        d2 <- digit
        return (10 * d1 + d2)


tests2 :: [IO Bool]
tests2 = [
  run "" (No 1),
  run "x" (No 1),
  run "5" (Yes 5),
  run "5x" (No 2),
  run "5xy" (No 2),
  run "56" (Yes 56),
  run "56x" (No 3),
  run "567" (Yes 567)
  ]
  where
    tag = "num"
    (run,_runX) = runTest tag lang 
    lang = do
      tok <- token
      let digit = digitTok tok
      fix"N" $ \num -> return$ alts [
        digit,
        do
          n <- num
          d <- digit
          return (10 * n + d)
        ]


data E = Leaf Int | Add E E deriving (Show,Eq)

tests3 :: [IO Bool]
tests3 = [
  run "5" (Yes (Leaf 5)),
  run "(5)" (Yes (Leaf 5)),
  run "((5))" (Yes (Leaf 5)),
  run "5+6" (Yes (Add (Leaf 5) (Leaf 6))),
  run "5+6+7" (Amb 2 [Add (Leaf 5) (Add (Leaf 6) (Leaf 7)), Add (Add (Leaf 5) (Leaf 6)) (Leaf 7)]),
  run "(5+6)+7" (Yes (Add (Add (Leaf 5) (Leaf 6)) (Leaf 7))),
  run "5+(6+7)" (Yes (Add (Leaf 5) (Add (Leaf 6) (Leaf 7)))),
  run "xy" (No 1),
  run "(xy" (No 2),
  run "(5" (No 3),
  run ")x" (No 1),
  run "+5" (No 1),
  run "5+" (No 3),
  run "5++6" (No 3),
  run "56" (No 2),
  run "56+7" (No 2),
  run "5)x" (No 2),
  run "" (No 1)
  ]
  where
    tag = "expD"
    (run,_runX) = runTest tag lang 
    lang = do
      tok <- token
      let digit = digitTok tok
      let sym = symTok tok
      fix"E" $ \exp -> return $ alts [
        do d <- digit; return (Leaf d),
        do e1 <- exp; sym '+' ; e2 <- exp; return (Add e1 e2),
        do sym '(' ; e <- exp; sym ')'; return e
        ]


tests4 :: [IO Bool]
tests4 = [
  run "5" (Yes (Leaf 5)),
  run "(5)" (Yes (Leaf 5)),
  run "((5))" (Yes (Leaf 5)),
  run "5+6" (Yes (Add (Leaf 5) (Leaf 6))),
  run "5+6+7" (Amb 2 [Add (Leaf 5) (Add (Leaf 6) (Leaf 7)),Add (Add (Leaf 5) (Leaf 6)) (Leaf 7)]),
  run "(5+6)+7" (Yes (Add (Add (Leaf 5) (Leaf 6)) (Leaf 7))),
  run "5+(6+7)" (Yes (Add (Leaf 5) (Add (Leaf 6) (Leaf 7)))),
  run "5" (Yes (Leaf 5)),
  run "51" (Yes (Leaf 51)),
  run "(5)" (Yes (Leaf 5)),
  run "(51)" (Yes (Leaf 51)),
  run "51+61" (Yes (Add (Leaf 51) (Leaf 61))),
  run "51+61+71" (Amb 2 [Add (Leaf 51) (Add (Leaf 61) (Leaf 71)),Add (Add (Leaf 51) (Leaf 61)) (Leaf 71)]),
  run "(51+61)+71" (Yes (Add (Add (Leaf 51) (Leaf 61)) (Leaf 71))),
  run "51+(61+71)" (Yes (Add (Leaf 51) (Add (Leaf 61) (Leaf 71)))),
  run "xy" (No 1),
  run "(xy" (No 2),
  run "(5" (No 3),
  run ")x" (No 1),
  run "+5" (No 1),
  run "5+" (No 3),
  run "5++6" (No 3),
  run "5)x" (No 2),
  run "" (No 1)
  ]
  where
    tag = "expN"
    (run,_runX) = runTest tag lang 
    lang = do
      tok <- token
      let digit = digitTok tok
      let sym = symTok tok
      num <- fix"N" $ \num -> return $ alts [
        digit,
        do n <- num; d <- digit; return (10 * n + d)
        ]
      exp <- fix"E" $ \exp -> return $ alts [
        do d <- num; return (Leaf d),
        do e1 <- exp; sym '+' ; e2 <- exp; return (Add e1 e2),
        do sym '(' ; e <- exp; sym ')'; return e
        ]
      return exp


-- counting outcomes...

tests5 :: [IO Bool]
tests5 = [
  run "" 0,
  run "a" 0,
  run "ab" 1,
  run "abc" 2,
  run "abcd" 5,
  run "abcde" 14,
  run "abcdef" 42
  ]
  where
    tag = "catalan"
    (run,_runX) = runTestParseThen allowAmb countOutcomes tag lang 
    lang = do
      tok <- token
      let x = do _ <- tok; return ()
      fix"cat" $ \u -> return $ alts [
        do x; x,
        do x; u,
        do u; x,
        do u; u
        ]

tests6 :: [IO Bool]
tests6 = [
  run "" 0,
  run "a" 0,
  run "ab" 0
  ]
  where
    tag = "zeroG"
    (run,_runX) = runTestParseThen allowAmb countOutcomes tag lang 
    lang = do
      return (do (fail :: Gram Int))


tests7 :: [IO Bool]
tests7 = [
  run "" 1,
  run "a" 0,
  run "ab" 0
  ]
  where
    tag = "unitG"
    (run,_runX) = runTestParseThen allowAmb countOutcomes tag lang 
    lang = do
      return (do return ())


-- effort...

tests8 :: [IO Bool]
tests8 = [ -- LINEAR: 2 + 4n
  run "" 2,
  run "a" 6,
  run "ab" 10,
  run "abc" 14,
  run "abcd" 18
  ]
  where
    tag = "left-recursion-effort"
    (run,_runX) = runTestParseThen allowAmb measureEffort tag lang 
    lang = do
      tok <- token
      let x = do _ <- tok; return ()
      fix"L" $ \xs -> return $ alts [
        x,
        do xs; x -- Left Recursion
        ]


tests9 :: [IO Bool]
tests9 = [ -- STILL QUADRATIC (n^2 + 7n + 2) / 2
  run "" 1,
  run "a" 5,
  run "ab" 10,
  run "abc" 16,
  run "abcd" 23
  ]
  where
    tag = "right-recursion-effort"
    (run,_runX) = runTestParseThen allowAmb measureEffort tag lang 
    lang = do
      tok <- token
      let x = do _ <- tok; return ()
      fix"R" $ \xs -> return $ alts [
        x,
        do x; xs -- Right Recursion
        ]


tests :: [IO Bool]
tests = concat [
  tests1, tests2,
  tests3, tests4,
  tests5,
  tests6, tests7,
  tests8, tests9,
  []
  ]

