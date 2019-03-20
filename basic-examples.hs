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

countAmbiguity :: Outcome a -> Int
countAmbiguity  o = case o of
  No _ -> 0
  Ambiguous _ -> 0
  Yes _ -> 1
  Multiple n _ -> n

measureAmbiguity :: Parsing a -> Int
measureAmbiguity (Parsing _ _ o) = countAmbiguity o

measureEffort :: Parsing a -> Int
measureEffort (Parsing (Eff e) _ _) = e

measureEffortAndAmbiguity :: Parsing a -> (Int,Int)
measureEffortAndAmbiguity (Parsing (Eff e) _ o) = (e, countAmbiguity o)

                           
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
  run "5+6+7" (Ambiguous (Ambiguity "E" 0 5)),
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
  run "51" (Yes (Leaf 51)),
  run "(51)" (Yes (Leaf 51)),
  run "51+61" (Yes (Add (Leaf 51) (Leaf 61))),
  run "51+61+71" (Ambiguous (Ambiguity "E" 0 7)),
  run "(51+61)+71" (Yes (Add (Add (Leaf 51) (Leaf 61)) (Leaf 71))),
  run "51+(61+71)" (Yes (Add (Leaf 51) (Add (Leaf 61) (Leaf 71)))),
  run "xy" (No 1),
  run "(xy" (No 2),
  run "(51" (No 4),
  run ")x" (No 1),
  run "+51" (No 1),
  run "51+" (No 4),
  run "51++61" (No 4),
  run "51)x" (No 3),
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
    (run,_runX) = runTestParseThen allowAmb measureAmbiguity tag lang 
    seq g1 g2 = do x1 <- g1; x2 <- g2; return$ "("++x1++x2++")"
    lang = do
      tok <- token
      let x = do c <- tok; return [c]
      fix"cat" $ \c -> return $ alts [
        seq x x,
        seq x c,
        seq c x,
        seq c c
        ]

tests6 :: [IO Bool]
tests6 = [
  run "" 0,
  run "a" 0,
  run "ab" 0
  ]
  where
    tag = "zeroG"
    (run,_runX) = runTestParseThen allowAmb measureAmbiguity tag lang 
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
    (run,_runX) = runTestParseThen allowAmb measureAmbiguity tag lang 
    lang = do
      return (do return ())


-- check effort for left/right recursions

tests8 :: [IO Bool]
tests8 = [ 
  run "" 4,
  run "a" 8, -- +4..
  run "ab" 12,
  run "abc" 16,
  run "abcd" 20
  ]
  where
    tag = "left-recursion"
    -- LINEAR
    (run,_runX) = runTestParseThen allowAmb measureEffort tag lang 
    lang = do
      tok <- token
      let x = do _ <- tok; return ()
      fix"L" $ \xs -> return $ alts [x, do xs; x]


tests9 :: [IO Bool]
tests9 = [ 
  run "" 2,
  run "a" 6, -- +4,5,6,7..
  run "ab" 11,
  run "abc" 17,
  run "abcd" 24
  ]
  where
    tag = "right-recursion"
    -- QUADRATIC -- expected.
    (run,_run) = runTestParseThen allowAmb measureEffort tag lang 
    lang = do
      tok <- token
      let x = do _ <- tok; return ()
      fix"R" $ \xs -> return $ alts [x, do x; xs]


tests10 :: [IO Bool]
tests10 = [ 
  run "." 6,
  run "a." 10, -- +4..
  run "ab." 14,
  run "abc." 18,
  run "abcd." 22
  ]
  where
    tag = "right-recursion-marked-termination"
    -- LINEAR
    (run,_run) = runTestParseThen allowAmb measureEffort tag lang 
    lang = do
      tok <- token
      let mark = do c <- tok; if c=='.' then return () else fail
      let x = do _ <- tok; return ()
      fix"R" $ \xs -> return $ alts [mark, do x; xs]


tests11 :: [IO Bool]
tests11 = [
  run "a" 3,
  run "ab" 5, -- +2..
  run "abc" 7,
  run "abcd" 9
  ]
  where
    tag = "unfolding-right-recursion"
    -- LINEAR -- Even when we measure the internal steps. Still kind of suprised.
    (run,_run) = runTestParseThen allowAmb measureEffort tag lang
    many p = alts [return [], do x <- p; xs <- many p; return (x : xs)]
    lang = do
      tok <- token
      let x = do _ <- tok; return ()
      return$ do
        x; _ <- many x; return ()


tests12 :: [IO Bool]
tests12 = [

  -- LINEAR in the length of the input 
  run "1;2" (24,1),
  run "12;34" (36,1), -- +12..
  run "123;456" (48,1),
  run "1234;5678" (60,1),
  
  -- where the semi colons are seems not to matter (or to matter only linearly)
  run "1;2345678" (72,1),
  run "12;345678" (68,1), -- -2..
  run "123;45678" (64,1),
  run "1234;5678" (60,1),

  -- LINEAR in the amount of ambiguity (when semicolon added at the start)
  run "12345678" (36,0),
  run "1;2345678" (72,1), -- +36..
  run "1;2;345678" (108,2),
  run "1;2;3;45678" (144,3),
  run "1;2;3;4;5678" (180,4),

  -- but QUADRATIC when semicolons are added at the back
  run "12345678" (36,0),
  run "1234567;8" (48,1), -- +12,20,28..
  run "123456;7;8" (68,2),
  run "12345;6;7;8" (96,3),
  
  run "" (4,0)
  ]
  where
    tag = "ambiguous-list-splits"
    (run,_run) = runTestParseThen allowAmb measureEffortAndAmbiguity tag lang
    lang = do
      tok <- token
      let x = do _ <- tok; return ()
      xs <- fix"L" $ \xs -> return $ alts [x, do xs; x]
      let semi = do c <- tok; if c==';' then return () else fail
      return$ do
        xs; semi; xs



tests13 :: [IO Bool]
tests13 = [
  check "",
  check "a",
  check "ab",
  check "abc",
  check "abcd"
  ]
  where
    tag = "everything"
    check input = run input (Yes input)
    (run,_run) = runTest tag lang 
    lang :: Lang Char (Gram String)
    lang = do
      tok <- token
      fix"L" $ \list -> return $ alts [
        do xs <- list; x <- tok; return (xs++[x]),
        return []
        ]

tests14 :: [IO Bool]
tests14 = [
  check "",
  check "a",
  check "ab",
  check "abc",
  check "abcd"
  ]
  where
    tag = "everything/read-pipe-existing-elems"
    check input = run input (Yes input)
    (run,_run) = runTest tag lang 
    lang :: Lang Char (Gram String)
    lang = do
      tok <- token
      fix"L" $ \list -> return $ alts [-- 2 alts reversed
        return [],
        do xs <- list; x <- tok; return (xs++[x])
        ]


tests15 :: [IO Bool]
tests15 = [
  check "a" 2,
  check "ab" 4,
  check "abc" 8,
  check "abcd" 16
  ]
  where
    tag = "everything/exponentially-repeated"
    check input n = run input (Multiple n (take n (repeat input)))
    (run,_run) = runTestAllowAmb tag lang 
    lang :: Lang Char (Gram String)
    lang = do
      tok <- token
      fix"L" $ \list -> return $ alts [
        do xs <- list; x <- tok; return (xs++[x]),
        return [],
        do xs <- list; x <- tok; return (xs++[x])
        ]



tests :: [IO Bool]
tests = concat [
  tests1, tests2,
  tests3, tests4,
  tests5,
  tests6, tests7,
  tests8, tests9, tests10, tests11,
  tests12,
  tests13, tests14, tests15,
  []
  ]

