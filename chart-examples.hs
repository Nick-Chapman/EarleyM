module ChartExamples(tests) where
-- TODO: port these examples to NewLang styleinterface

import Prelude hiding (fail)
import qualified Data.Char as Char
import Chart
import Testing

no :: Pos -> Outcome a --ignore the Pos we oneday might like
no _ = No

check :: (Show t, Show a, Eq a) => String -> Lang t a -> [t] -> Outcome a -> IO Bool
check tag lang input expect =
  let actual = parse lang input in
  printCompare tag input actual expect

checkAmb :: (Show t) => String -> Lang t a -> [t] -> Int -> IO Bool
checkAmb tag lang input expect =
  let actual = countOutcomes (parse lang input) in
  printCompare tag input actual expect
  where
    countOutcomes o = case o of
      No -> 0
      Yes _ -> 1
      Amb n _ -> n

checkParseEffort :: (Show t) => String -> Lang t a -> [t] -> Int -> IO Bool
checkParseEffort tag lang input expect =
  let actual = parseEffort lang input in
  printCompare tag input actual expect


digitOfChar :: Char -> Int
digitOfChar c = Char.ord c - ord0
  where ord0 = Char.ord '0'

digitTok :: NT Char -> Gram Int
digitTok tok = 
  do c <- get tok; if Char.isDigit c then return (digitOfChar c) else fail

symTok :: NT Char -> Char -> Gram ()
symTok tok x = do c <- get tok; if c == x then return () else fail


tests1 :: [IO Bool]
tests1 =
  let run = check "2dig" lang in
  [
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
    lang =
      withNT $ \tok ->
      let digit = digitTok tok in
      withNT $ \num ->
      mkLang tok (get num) [
        produce num [
           do
             d1 <- digit
             d2 <- digit
             return (10 * d1 + d2)
           ]
        ]


tests2 :: [IO Bool]
tests2 =
  let run = check "num" lang in
  [
    run "" (no 0),
    run "x" (no 1),
    run "5" (Yes 5),
    run "5x" (no 2),
    run "56" (Yes 56),
    run "56x" (no 3),
    run "567" (Yes 567)
  ]
  where
    lang =
      withNT $ \tok ->
      let digit = digitTok tok in
      withNT $ \num ->
      mkLang tok (get num) [
        produce num [
           digit,
           do
             n <- get num
             d <- digit
             return (10 * n + d)
           ]
        ]



data E = Leaf Int | Add E E deriving (Show,Eq)

tests3 :: [IO Bool]
tests3 =
  let run = check "expD" lang in
  [
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
    lang =
      withNT $ \tok ->
      let digit = digitTok tok in
      let sym = symTok tok in
      withNT $ \exp ->
      mkLang tok (get exp) [
        produce exp [
           do d <- digit; return (Leaf d),
           do e1 <- get exp; sym '+' ; e2 <- get exp; return (Add e1 e2),
           do sym '(' ; e <- get exp; sym ')'; return e
           ]
        ]


tests4 :: [IO Bool]
tests4 =
  let run = check "expN" lang in
  [
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
    lang =
      withNT $ \tok ->
      let digit = digitTok tok in
      let sym = symTok tok in
      withNT $ \num ->
      withNT $ \exp ->
      mkLang tok (get exp) [
        produce num [
           digit,
           do n <- get num; d <- digit; return (10 * n + d)
           ],
        produce exp [
          do n <- get num; return (Leaf n),
          do e1 <- get exp; sym '+' ; e2 <- get exp; return (Add e1 e2),
          do sym '(' ; e <- get exp; sym ')'; return e
          ]
        ]


tests5 :: [IO Bool]
tests5 =
  let run = checkAmb "catalan" lang in
  [
    run "" 0,
    run "a" 0,
    run "ab" 1,
    run "abc" 2,
    run "abcd" 5,
    run "abcde" 14,
    run "abcdef" 42
  ]
  where
    lang =
      withNT $ \tok ->
      let x = do _ <- get tok; return () in
      withNT $ \unit ->
      let u = get unit in
      mkLang tok (get unit) [
        produce unit [
           do x; x,
           do x; u,
           do u; x,
           do u; u
           ]
        ]


tests6 :: [IO Bool]
tests6 =
  let run = checkParseEffort "left" lang in
  [
    run "" 0,
    run "a" 2,
    run "ab" 6,
    run "abc" 12,
    run "abcd" 20
  ]
  -- quadratic! the hope is that this will become linear when we implement chart2
  -- STILL quadratic!!
  where
    lang =
      withNT $ \tok ->
      let x = do _ <- get tok; return () in
      withNT $ \unit ->
      let xs = get unit in
      mkLang tok (get unit) [
        produce unit [
           x,
           do xs; x -- Left Recursion
           ]
        ]


tests7 :: [IO Bool]
tests7 =
  let run = checkParseEffort "right" lang in
  [
    run "" 0,
    run "a" 2,
    run "ab" 6,
    run "abc" 12,
    run "abcd" 20
  ]
  -- quadratic! the hope is that this will become linear when we implement chart2
  -- STILL quadratic!! 2*triangle(n)
  where
    lang =
      withNT $ \tok ->
      let x = do _ <- get tok; return () in
      withNT $ \list ->
      let xs = get list in
      mkLang tok (get list) [
        produce list [
           x,
           do x; xs -- Left Recursion
           ]
        ]


tests8 :: [IO Bool]
tests8 =
  let run = checkAmb "zero" lang in
  [
    run "" 0,
    run "a" 0,
    run "ab" 0
  ]
  where
    lang =
      withNT $ \tok ->
      withNT $ \start ->
      mkLang tok (get start) [
        produce start []
        ]


tests9 :: [IO Bool]
tests9 =
  let run = checkAmb "unit" lang in
  [
    run "" 1,
    run "a" 0,
    run "ab" 0
  ]
  where
    lang =
      withNT $ \tok ->
      withNT $ \start ->
      mkLang tok (get start) [
        produce start [ return () ]
        ]


tests :: [IO Bool]
tests = concat [
  tests1,
  tests2,
  tests3,
  tests4,
  tests5,
  tests6,tests7,
  tests8,tests9
  ]
