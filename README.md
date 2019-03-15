# haskell-chart-parsers

Haskell implementation of Monadic-style combinators for chart parsing.

These combinators support left-recursive and ambiguous grammars - but feel like plain old monadic parser combinators.

This Haskell implementation re-work an earlier Ocaml implementation:
https://github.com/Nick-Chapman/ocaml-chart-parsers

But with the following changes:

* Purely functional implementation - of course, this is Haskell!

* Improved interface: No need to union the types of all non-terminals. Two monadic layers, for: Gram, Lang

* Lots of unit tests...

* Including a fleshed out example of an expression language, supporting:
    * identifiers, numbers
    * left associating binary operator
    * function application by juxtaposition
    * lambda abstraction

Demonstrating:
    * Parsing without a separate lexer (this parser works directly on the characters as tokens).
    * Handling whitespace correctly, without the need to peek ahead:
        * "f 1x"  is rejected
        * "f(1)x" is accepted

Summary of the Haskell interface: (via NewLang):

```haskell
data NT a
data Gram a
data Lang t a

instance Monad Gram
instance Monad (Lang t)

fail :: Gram a
alts :: [Gram a] -> Gram a
token :: Lang t (Gram t)

declare :: Lang t (NT a, Gram a)
produce :: NT a -> Gram a -> Lang t ()

data Outcome a = Yes a | Amb Int [a] | No -- TODO: Position info
parse :: Lang t (Gram a) -> [t] -> Outcome a

fix :: (Gram a -> Lang t (Gram a)) -> Lang t (Gram a) -- convenience, defined using declare/produce
```

Build/run (from the jbuild):
```
ghc -Wall -fno-warn-name-shadowing --make main new-lang chart pipe testing chart-examples new-lang-example juxta-exp-example oleg-example -o main.exe
./main.exe 
```
