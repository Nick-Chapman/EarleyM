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

no :: Pos -> Outcome a --ignore the Pos we oneday might like
no _ = No

countOutcomes :: Outcome a -> Int
countOutcomes o = case o of
  No -> 0
  Yes _ -> 1
  Amb n _ -> n


tests1 :: [IO Bool]
tests1 = [
  run "" (no 0), -- run-out, so is (no 1) better ?
  run "x" (no 1),
  run "xy" (no 1),
  run "5" (no 1), -- run-out, so is (no 2) better ?
  run "5x" (no 2),
  run "5xy" (no 2),
  run "56" (Yes 56),
  run "56x" (no 3),
  run "56xy" (no 3),
  run "56xyz" (no 3),
  run "567" (no 3)
  ]
  where
    tag = "2dig"
    run input expect = printCompare tag input (parse lang input) expect
    lang = do
      tok <- token
      let digit = digitTok tok
      return $ do
        d1 <- digit
        d2 <- digit
        return (10 * d1 + d2)


tests2 :: [IO Bool]
tests2 = [
  run "" (no 0),
  run "x" (no 1),
  run "5" (Yes 5),
  run "5x" (no 2),
  run "56" (Yes 56),
  run "56x" (no 3),
  run "567" (Yes 567)
  ]
  where
    tag = "num"
    run input expect = printCompare tag input (parse lang input) expect
    lang = do
      tok <- token
      let digit = digitTok tok
      fix $ \num -> return$ alts [
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
  run "5+6+7" (Amb 2 [Add (Add (Leaf 5) (Leaf 6)) (Leaf 7),Add (Leaf 5) (Add (Leaf 6) (Leaf 7))]),
  run "(5+6)+7" (Yes (Add (Add (Leaf 5) (Leaf 6)) (Leaf 7))),
  run "5+(6+7)" (Yes (Add (Leaf 5) (Add (Leaf 6) (Leaf 7)))),
  run "xy" (no 1),
  run "(xy" (no 2),
  run "(5" (no 2), --better 3?
  run ")x" (no 1),
  run "+5" (no 1),
  run "5+" (no 2),
  run "5++6" (no 3),
  run "56" (no 2),
  run "56+7" (no 4), -- what??? this should be 2! (chart2 will fix?!)
  run "5)x" (no 2),
  run "" (no 0)
  ]
  where
    tag = "expD"
    run input expect = printCompare tag input (parse lang input) expect
    lang = do
      tok <- token
      let digit = digitTok tok
      let sym = symTok tok
      fix $ \exp -> return $ alts [
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
  run "51+(61+71)" (Yes (Add (Leaf 51) (Add (Leaf 61) (Leaf 71)))),
  run "xy" (no 1),
  run "(xy" (no 2),
  run "(5" (no 2),
  run ")x" (no 1),
  run "+5" (no 1),
  run "5+" (no 2),
  run "5++6" (no 3),
  run "5)x" (no 2),
  run "" (no 0)
  ]
  where
    tag = "expN"
    run input = printCompare tag input (parse lang input)
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
    run input = printCompare tag input (countOutcomes (parse lang input))
    lang = do
      tok <- token
      let x = do _ <- tok; return ()
      fix $ \u -> return $ alts [
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
    run input = printCompare tag input (countOutcomes (parse lang input))
    lang = do
      return (do fail)


tests7 :: [IO Bool]
tests7 = [
  run "" 1,
  run "a" 0,
  run "ab" 0
  ]
  where
    tag = "unitG"
    run input = printCompare tag input (countOutcomes (parse lang input))
    lang = do
      return (do return ())


-- effort...

tests8 :: [IO Bool]
tests8 = [
  run "" 0,
  run "a" 2,
  run "ab" 6,
  run "abc" 12,
  run "abcd" 20
  ]
-- quadratic! the hope is that this will become linear when we implement chart2
-- STILL quadratic!! TODO...
  where
    tag = "left-recursion-effort"
    run input = printCompare tag input (parseEffort lang input)
    lang = do
      tok <- token
      let x = do _ <- tok; return ()
      fix $ \xs -> return $ alts [
        x,
        do xs; x -- Left Recursion
        ]


tests9 :: [IO Bool]
tests9 = [
  run "" 0,
  run "a" 2,
  run "ab" 6,
  run "abc" 12,
  run "abcd" 20
  ]
-- quadratic! the hope is that this will become linear when we implement chart2
-- STILL quadratic!! TODO...
  where
    tag = "right-recursion-effort"
    run input = printCompare tag input (parseEffort lang input)
    lang = do
      tok <- token
      let x = do _ <- tok; return ()
      fix $ \xs -> return $ alts [
        x,
        do xs; x -- Right Recursion
        ]


tests :: [IO Bool]
tests = concat [
  tests1, tests2, tests3, tests4, tests5,
  tests6, tests7, tests8, tests9
  ]
