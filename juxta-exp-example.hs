module JuxtaExpExample(tests) where

import Prelude hiding (fail,exp)
import qualified Data.Char as Char
import Testing
import Chart

digitOfChar :: Char -> Int
digitOfChar c = Char.ord c - ord0 where ord0 = Char.ord '0'

data Exp
  = Var String
  | Num Int
  | App Exp Exp   
  | Lam String Exp 
  | Add Exp Exp   
  deriving (Eq)

instance Show Exp where -- simple, fully parenthesized, pretty-printer
  show e = case e of
    Var s -> s
    Num i -> show i
    App e1 e2 -> "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    Lam s body -> "(\\" ++ s ++ "." ++ show body ++ ")"
    Add e1 e2 -> "(" ++ show e1 ++ "+" ++ show e2 ++ ")"

lang :: Lang Char (Gram Exp)

--data OpenOrClosedOnRight = Open | Closed deriving Show

lang = do

  sym <- symbol
  sat0 <- satisfy
  let sat name pred = sat0 name $ \c -> if pred c then Just c else Nothing
  let alpha = sat "alpha" Char.isAlpha
  let numer = sat "numer" Char.isDigit
  let digit = do c <- numer; return (digitOfChar c)
  let white = do _ <- sat "white" Char.isSpace; return ()
  digits <- fix"digits" $ \digits -> return $ alts [
    do n <- digits; d <- digit; return (10 * n + d),
    digit
    ]

  ident <- share "ident" $ do x <- alpha; xs <- many (alts [alpha,numer]); return (x : xs) 
  ws <- share "ws" (skipWhile white) -- optional white space

  let required_ws = do white; ws
  
  let var = do s <- ident; return (Var s)
  let num = do n <- digits; return (Num n)
  let parenthesized thing = do sym '('; ws; x <- thing; ws; sym ')'; return x

  --let lambda exp = do sym '\\'; ws; x <- ident; ws; sym '.'; ws; e <- exp; return (Lam x e)
  --let lambdarized exp = alts [exp, lambda (lambdarized exp)] -- right recursion
  
  let lambdarized exp = do
        xs <- many (do sym '\\'; ws; x <- ident; ws; sym '.'; ws; return x)
        e <- exp
        return$ foldr Lam e xs

  exp <- fix"exp" $ \exp -> do

    lambdarized_exp <- share "lambda" (lambdarized exp)
    --let lambdarized_exp = (lambdarized exp)
    
{-
    let leftOpenAtom =   do e <- alts [var,num];                  return (e,Open)
    let leftClosedAtom = do e <- parenthesized (lambdarized exp); return (e,Closed)
    let atom = alts [leftClosedAtom, leftOpenAtom]

    app <- fix"app" $ \app -> return $ alts [
      atom,
      do
        ~(a,oc1) <- app;
        gap <- alts [return False, do required_ws; return True]
        (b,oc2) <- case (oc1,gap) of (Open,False) -> leftClosedAtom; _ -> atom -- CONTEXT SENSITIVE
        return (App a b, oc2)
      ]
    let application = do ~(a,_) <- app; return a
-}

    -- must distingish open/closed atoms and applications

    atomO <- share "atomO" (alts [var,num])
    atomC <- share "atomC" (parenthesized (lambdarized_exp))

    (appC',appC) <- declare"appC"
    (appO',appO) <- declare"appO"
    
    produce appC' $ alts [
      atomC,
      do a <- appC; ws; b <- atomC; return (App a b),
      do a <- appO; ws; b <- atomC; return (App a b)
      ]
    
    produce appO' $ alts [
      atomO,
      do a <- appC;          ws; b <- atomO; return (App a b),
      do a <- appO; required_ws; b <- atomO; return (App a b)
      ]
      
    let application = alts [appO,appC]

    let addition = alts [
          application,
          do a <- exp; ws; sym '+'; ws; b <- exp; return (Add a b) -- grammar is deliberately ambiguous here
          ]

    return addition
    
  let start = do ws; e <- lambdarized exp; ws; return e
  return start


tests1 :: [IO Bool]
tests1 = [

  run "a"           "Yes a",
  run "(a)"         "Yes a",
  run "ab"          "Yes ab",
  run "1"           "Yes 1",
  run "12"          "Yes 12",
  run "a1"          "Yes a1",
  run "1a"          "No 2", --No here

  run "a b"         "Yes (a b)",
  run "(a)b"        "Yes (a b)",
  run "a(b)"        "Yes (a b)",
  run "1 2"         "Yes (1 2)", -- type silly

  run "a b c"       "Yes ((a b) c)",
  run "a(b)c"       "Yes ((a b) c)",
  run "(a b)c"      "Yes ((a b) c)",
  run "(a b) c"     "Yes ((a b) c)",
  
  run "a(b c)"      "Yes (a (b c))",
  run "a (b c)"     "Yes (a (b c))",
  
  run "a+b"         "Yes (a+b)",
  run " a + b "     "Yes (a+b)",

  run "a+b+c "      "Ambiguous (Ambiguity \"exp\" 0 5)",
  run "(a+b)+c"     "Yes ((a+b)+c)",
  run "a+(b+c)"     "Yes (a+(b+c))",
  
  run "f(1+2+3)x"   "Ambiguous (Ambiguity \"exp\" 2 7)",
  run "f((1+2)+3)x" "Yes ((f ((1+2)+3)) x)",
  run "f(1+(2+3))x" "Yes ((f (1+(2+3))) x)",

  run "f(1+2+3+4)x" "Ambiguous (Ambiguity \"exp\" 2 7)",

  
  -- examples originally copied from parser4v tests

  run "4"           "Yes 4",
  run "42"          "Yes 42",
  run "4 "          "Yes 4",
  run " 4"          "Yes 4",
  run "x"           "Yes x",
  run "xy"          "Yes xy",
  run "x4"          "Yes x4",
  run "x y"         "Yes (x y)",
  run "  x  y  "    "Yes (x y)",
  run "x y z"       "Yes ((x y) z)",
  run "x 4"         "Yes (x 4)",
  run "x 4 y"       "Yes ((x 4) y)",
  run "4 y"         "Yes (4 y)", --type silly
  run "(4)"         "Yes 4",
  run " ( 4 ) "     "Yes 4",
  run "((4))"       "Yes 4",
  run "x (y)"       "Yes (x y)",
  run "(x) y"       "Yes (x y)",
  run "(x) (y)"     "Yes (x y)",
  run "((x) (y))"   "Yes (x y)",
  run "(x y) z"     "Yes ((x y) z)",
  run "x (y z)"     "Yes (x (y z))",
  run "1+2"         "Yes (1+2)",
  run " 1 + 2 "     "Yes (1+2)",
  run "(1+2)+3"     "Yes ((1+2)+3)",
  run "1+(2+3)"     "Yes (1+(2+3))",

  run "\\x.x"           "Yes (\\x.x)",
  run " \\ x . x "      "Yes (\\x.x)",
  run "\\x.\\y.x 1"     "Yes (\\x.(\\y.(x 1)))",
  run "\\x.(\\y.x) 1"   "Yes (\\x.((\\y.x) 1))",
  run "(\\x.\\y.x) 1"   "Yes ((\\x.(\\y.x)) 1)",

  -- would like a more flexible approach to the syntax of lambda, w,r,t the interaction with app/add
  -- but it's really tricky to write the grammar !
  {-
  run "f \\x.x"         "Yes (f (\\x.x))",
  run "f \\x.x+y"       "Yes (f (\\x.(x+y)))",
  run "f\\x.g\\y.k x y" "Yes (f (\\x.(g (\\y.((k x) y)))))",
  run "f+\\x.x"         "Yes (f+(\\x.x))",
  run "f+\\x.x+y"       "Yes (f+(\\x.(x+y)))",
  run "f+\\x.x+\\y.y"   "Yes (f+(\\x.(x+(\\y.y))))",
-}
  -- so here we have example with the currently necesssary parens added...
  run "f(\\x.x)"            "Yes (f (\\x.x))",
  run "f(\\x.x+y)"          "Yes (f (\\x.(x+y)))",
  run "f(\\x.g(\\y.k x y))" "Yes (f (\\x.(g (\\y.((k x) y)))))",
  run "f+(\\x.x)"           "Yes (f+(\\x.x))",
  run "f+(\\x.x+y)"         "Yes (f+(\\x.(x+y)))",
  run "f+(\\x.x+(\\y.y))"   "Yes (f+(\\x.(x+(\\y.y))))",
  
  run "(\\f.\\x.f(f x))(\\x.x+1)5"                       "Yes (((\\f.(\\x.(f (f x)))) (\\x.(x+1))) 5)",
  run " ( \\ f . \\ x . f ( f x ) ) ( \\ x . x + 1 ) 5 " "Yes (((\\f.(\\x.(f (f x)))) (\\x.(x+1))) 5)",

  run "f quitelong 3" "Yes ((f quitelong) 3)",
  
  run " "   "No 2",
  run "#"   "No 1",
  run ")"   "No 1",
  run "("   "No 2",
  run "()"  "No 2",
  run "4)"  "No 2",
  run "4#"  "No 2",
  run "4x"  "No 2",
  run "42x" "No 3",

  run "foo arg (\\x.42) + 10" "Yes (((foo arg) (\\x.42))+10)",
  
  run "@foo arg (\\x.42) + 10" "No 1",
  run "f@oo arg (\\x.42) + 10" "No 2",
  run "fo@o arg (\\x.42) + 10" "No 3",
  run "foo@ arg (\\x.42) + 10" "No 4",
  run "foo @arg (\\x.42) + 10" "No 5",
  run "foo a@rg (\\x.42) + 10" "No 6",
  run "foo ar@g (\\x.42) + 10" "No 7",
  run "foo arg@ (\\x.42) + 10" "No 8",
  run "foo arg @(\\x.42) + 10" "No 9",
  run "foo arg (@\\x.42) + 10" "No 10",
  run "foo arg (\\@x.42) + 10" "No 11",
  run "foo arg (\\x@.42) + 10" "No 12",
  run "foo arg (\\x.@42) + 10" "No 13",
  run "foo arg (\\x.4@2) + 10" "No 14",
  run "foo arg (\\x.42@) + 10" "No 15",
  run "foo arg (\\x.42)@ + 10" "No 16",
  run "foo arg (\\x.42) @+ 10" "No 17",
  run "foo arg (\\x.42) +@ 10" "No 18",
  run "foo arg (\\x.42) + @10" "No 19",
  run "foo arg (\\x.42) + 1@0" "No 20",
  run "foo arg (\\x.42) + 10@" "No 21",

  run ""    "No 1"
  ]
  where
    tag = "juxta-exp"
    (run,_runX) = runTestParseThen rejectAmb (\(Parsing _ _ o) -> show o) tag lang


-- test option to reject ambigious parses, reporting the Ambiguity NT and location
tests2 :: [IO Bool]
tests2 = [
  run "a+b+c"       "Multiple 2 [((a+b)+c),(a+(b+c))]",
  run "(a+b)+c"     "Yes ((a+b)+c)",
  run "a+(b+c)"     "Yes (a+(b+c))",
  
  run "f(1+2+3)x"   "Multiple 2 [((f ((1+2)+3)) x),((f (1+(2+3))) x)]",
  run "f((1+2)+3)x" "Yes ((f ((1+2)+3)) x)",
  run "f(1+(2+3))x" "Yes ((f (1+(2+3))) x)",

  run "f(1+2+3+4)x" "Multiple 5 [((f (((1+2)+3)+4)) x),((f ((1+2)+(3+4))) x),((f (1+(2+(3+4)))) x),((f ((1+(2+3))+4)) x),((f (1+((2+3)+4))) x)]"
  
  ]
  where
    tag = "juxta-exp-no-amb"
    (run,_runX) = runTestParseThen allowAmb (\(Parsing _ _ o) -> show o) tag lang


tests :: [IO Bool]
tests = concat [
  [do print (mkStaticLang lang); return True],
  tests1,
  tests2,
  []
  ]

  
