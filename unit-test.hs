module UnitTest(tests) where

import Prelude hiding (fail,exp,seq)
import qualified Data.Char as Char
import Testing
import Earley


getDigit :: Lang Char (Gram Char Int)
getDigit = do
  token <- getToken
  return$
    do c <- token; if Char.isDigit c then return (digitOfChar c) else fail
  where
    digitOfChar :: Char -> Int
    digitOfChar c = Char.ord c - ord0 where ord0 = Char.ord '0'


err :: Pos -> Either ParseError b
err pos = Left (SyntaxError (UnexpectedTokenAt pos))

incomplete :: Pos -> Either ParseError b
incomplete pos = Left (SyntaxError (UnexpectedEOF pos))

amb :: Ambiguity -> Either ParseError b
amb a = Left (AmbiguityError a)

           
tests1 :: [IO Bool]
tests1 = [
  run "" (incomplete 1),
  run "x" (err 1),
  run "xy" (err 1),
  run "5" (incomplete 2),
  run "5x" (err 2),
  run "5xy" (err 2),
  run "56" (Right 56),
  run "56x" (Left (SyntaxError (ExpectedEOF 3))),
  run "56xy" (Left (SyntaxError (ExpectedEOF 3)))
  ]
  where
    tag = "2dig"
    run = check (outcome . parse lang) tag
    lang = do
      digit <- getDigit
      return $ do
        d1 <- digit
        d2 <- digit
        return (10 * d1 + d2)


tests2 :: [IO Bool]
tests2 = [
  run "" (incomplete 1),
  run "x" (err 1),
  run "5" (Right 5),
  run "5x" (err 2),
  run "5xy" (err 2),
  run "56" (Right 56),
  run "56x" (err 3),
  run "567" (Right 567)
  ]
  where
    tag = "num"
    run = check (outcome . parse lang) tag
    lang = do
      digit <- getDigit
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
  run "5" (Right (Leaf 5)),
  run "(5)" (Right (Leaf 5)),
  run "((5))" (Right (Leaf 5)),
  run "5+6" (Right (Add (Leaf 5) (Leaf 6))),
  run "5+6+7" (amb (Ambiguity "E" 0 5)),
  run "(5+6)+7" (Right (Add (Add (Leaf 5) (Leaf 6)) (Leaf 7))),
  run "5+(6+7)" (Right (Add (Leaf 5) (Add (Leaf 6) (Leaf 7)))),
  run "xy" (err 1),
  run "(xy" (err 2),
  run "(5" (incomplete 3),
  run ")x" (err 1),
  run "+5" (err 1),
  run "5+" (incomplete 3),
  run "5++6" (err 3),
  run "56" (err 2),
  run "56+7" (err 2),
  run "5)x" (err 2),
  run "" (incomplete 1)
  ]
  where
    tag = "expD"
    run = check (outcome . parse lang) tag
    lang = do
      token <- getToken
      digit <- getDigit
      let symbol x = do t <-token; if t==x then return () else fail
      fix"E" $ \exp -> return $ alts [
        do d <- digit; return (Leaf d),
        do e1 <- exp; symbol '+' ; e2 <- exp; return (Add e1 e2),
        do symbol '(' ; e <- exp; symbol ')'; return e
        ]


tests4 :: [IO Bool]
tests4 = [
  run "51" (Right (Leaf 51)),
  run "(51)" (Right (Leaf 51)),
  run "51+61" (Right (Add (Leaf 51) (Leaf 61))),
  run "51+61+71" (amb (Ambiguity "E" 0 7)),
  run "(51+61)+71" (Right (Add (Add (Leaf 51) (Leaf 61)) (Leaf 71))),
  run "51+(61+71)" (Right (Add (Leaf 51) (Add (Leaf 61) (Leaf 71)))),
  run "xy" (err 1),
  run "(xy" (err 2),
  run "(51" (incomplete 4),
  run ")x" (err 1),
  run "+51" (err 1),
  run "51+" (incomplete 4),
  run "51++61" (err 4),
  run "51)x" (err 3),
  run "" (incomplete 1)
  ]
  where
    tag = "expN"
    run = check (outcome . parse lang) tag
    lang = do
      token <- getToken
      digit <- getDigit
      let symbol x = do t <-token; if t==x then return () else fail
      num <- fix"N" $ \num -> return $ alts [
        digit,
        do n <- num; d <- digit; return (10 * n + d)
        ]
      exp <- fix"E" $ \exp -> return $ alts [
        do d <- num; return (Leaf d),
        do e1 <- exp; symbol '+' ; e2 <- exp; return (Add e1 e2),
        do symbol '(' ; e <- exp; symbol ')'; return e
        ]
      return exp


-- counting outcomes...

countAmb :: Either e [a] -> Int
countAmb (Left _) = 0
countAmb (Right xs) = length xs

tests6 :: [IO Bool]
tests6 = [
  run "" 0,
  run "a" 0,
  run "ab" 0
  ]
  where
    tag = "zeroG"
    run = check (countAmb . outcome . parseAmb lang) tag
    lang = do
      return (do (fail :: Gram Char Int))


tests7 :: [IO Bool]
tests7 = [
  run "" 1,
  run "a" 0,
  run "ab" 0
  ]
  where
    tag = "unitG"
    run = check (countAmb . outcome . parseAmb lang) tag
    lang = do
      return (do return ())


-- checkIO effort for left/right recursions


unEff :: Eff -> Int
unEff (Eff x) = x


tests8 :: [IO Bool]
tests8 = [ 
  run "" 3,
  run "a" 6, -- +3..
  run "ab" 9,
  run "abc" 12,
  run "abcd" 15
  ]
  where
    tag = "left-recursion"
    -- LINEAR
    run = check (unEff . effort . parse lang) tag
    lang = do
      token <- getToken
      let x = do _ <- token; return ()
      fix"L" $ \xs -> return $ alts [x, do xs; x]


tests9 :: [IO Bool]
tests9 = [ 
  run "" 3,
  run "a" 8, -- +5,6,7,8..
  run "ab" 14,
  run "abc" 21,
  run "abcd" 29
  ]
  where
    tag = "right-recursion"
    -- QUADRATIC -- expected.
    run = check (unEff . effort . parse lang) tag
    lang = do
      token <- getToken
      let x = do _ <- token; return ()
      fix"R" $ \xs -> return $ alts [x, do x; xs]


tests10 :: [IO Bool]
tests10 = [ 
  run "." 8,
  run "a." 12, -- +4..
  run "ab." 16,
  run "abc." 20,
  run "abcd." 24
  ]
  where
    tag = "right-recursion-marked-termination"
    -- LINEAR
    run = check (unEff . effort . parse lang) tag
    lang = do
      token <- getToken
      let symbol x = do t <-token; if t==x then return () else fail
      let x = do _ <- token; return ()
      fix"R" $ \xs -> return $ alts [symbol '.', do x; xs]


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
    run = check (unEff . effort . parse lang) tag
    lang = do
      token <- getToken
      let x = do _ <- token; return ()
      return$ do
        x; _ <- many x; return ()


tests12 :: [IO Bool]
tests12 = [

  -- LINEAR in the length of the input 
  run "1;2" (18,1),
  run "12;34" (27,1), -- +9..
  run "123;456" (36,1),
  run "1234;5678" (45,1),
  
  -- where the semi colons are seems not to matter (or to matter only linearly)
  run "1;2345678" (54,1),
  run "12;345678" (51,1), -- -3..
  run "123;45678" (48,1),
  run "1234;5678" (45,1),

  -- LINEAR in the amount of ambiguity (when semicolon added at the start)
  run "12345678" (27,0),
  run "1;2345678" (54,1), -- +27..
  run "1;2;345678" (81,2),
  run "1;2;3;45678" (108,3),
  run "1;2;3;4;5678" (135,4),

  -- but QUADRATIC when semicolons are added at the back
  run "12345678" (27,0),
  run "1234567;8" (36,1), -- +9,15,21
  run "123456;7;8" (51,2),
  run "12345;6;7;8" (72,3),
  
  run "" (3,0)
  ]
  where
    tag = "ambiguous-list-splits"
    run = check (measure . parseAmb lang) tag

    measure parsing = (unEff (effort parsing), countAmb (outcome parsing))
    
    lang = do
      token <- getToken
      let symbol x = do t <-token; if t==x then return () else fail
      let x = do _ <- token; return ()
      xs <- fix"L" $ \xs -> return $ alts [x, do xs; x]
      return$ do
        xs; symbol ';'; xs



tests13 :: [IO Bool]
tests13 = [
  run "",
  run "a",
  run "ab",
  run "abc",
  run "abcd"
  ]
  where
    tag = "everything"
    run input = check (outcome . parse lang) tag input (Right input)
    lang = do
      token <- getToken
      fix"L" $ \list -> return $ alts [
        do xs <- list; x <- token; return (xs++[x]),
        return []
        ]

tests14 :: [IO Bool]
tests14 = [
  run "",
  run "a",
  run "ab",
  run "abc",
  run "abcd"
  ]
  where
    tag = "everything/read-pipe-existing-elems"
    run input = check (outcome . parse lang) tag input (Right input)
    lang = do
      token <- getToken
      fix"L" $ \list -> return $ alts [-- 2 alts reversed
        return [],
        do xs <- list; x <- token; return (xs++[x])
        ]


tests15 :: [IO Bool]
tests15 = [
  run "a" 2,
  run "ab" 4,
  run "abc" 8,
  run "abcd" 16
  ]
  where
    tag = "everything/exponentially-repeated"
    run input n =
      check (outcome . parseAmb lang) tag input (Right (take n (repeat input)))
    lang = do
      token <- getToken
      fix"L" $ \list -> return $ alts [
        do xs <- list; x <- token; return (xs++[x]),
        return [],
        do xs <- list; x <- token; return (xs++[x])
        ]


tests :: [IO Bool]
tests = concat [
  tests1, tests2,
  tests3, tests4,
  tests6, tests7,
  tests8, tests9, tests10, tests11,
  tests12,
  tests13, tests14, tests15,
  []
  ]
