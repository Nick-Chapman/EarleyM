# earley-parsing

Monadic-style combinators for Earley Parsing.

These combinators support left-recursive and ambiguous grammars, but provide a standard monadic parser interface.
This implementation re-works an earlier Ocaml implementation:
https://github.com/Nick-Chapman/ocaml-chart-parsers
with the following changes:

* Purely functional implementation - of course, this is Haskell!

* Improved interface: No need to union the types of all non-terminals. Two monadic layers: Gram, Lang

* Lots of unit tests.

* Fleshed out example of an expression language, 
https://github.com/Nick-Chapman/Earley/blob/master/src/Example/FunctionalLanguage.hs
including:
    * identifiers, numbers
    * left associating binary operator
    * function application by juxtaposition
    * lambda abstraction
    
  Demonstrating:
    * Parsing without a separate lexer (this parser works directly on the characters as tokens).
    * Handling whitespace correctly, without the need to peek ahead:
        * "f 1x"  is rejected
        * "f(1)x" is accepted

Summary of the Haskell interface: (from NewLang):

```haskell
data NT a
data Gram a
data Lang t a

instance Monad Gram
instance Monad (Lang t)

fail :: Gram a
alts :: [Gram a] -> Gram a
token :: Lang t (Gram t)

declare :: String -> Lang t (NT a, Gram a)
produce :: NT a -> Gram a -> Lang t ()

data Outcome a = Yes a | Amb Int [a] | No -- TODO: Position info
parse :: Lang t (Gram a) -> [t] -> Outcome a

fix :: String -> (Gram a -> Lang t (Gram a)) -> Lang t (Gram a) -- convenience, defined using declare/produce
```

Build/run (from the jbuild):
```
ghc -Wall -fno-warn-name-shadowing *.hs -o main.exe
./main.exe
```
