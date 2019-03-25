module LostWhitespaceExample(test) where

import Prelude hiding (fail)
import qualified Data.Char as Char
import Testing
import Chart

-- This example constructs a very large grammar (from a dictionary), which is used to lex a sentence,
-- which has had it's interword whitespace removed, back into the original words, 
-- and also finds some alternative original sentences.

genLang :: [String] -> Lang Char (Gram [String])
genLang dict = do
  sym <- symbol
  let lit x = do sym x; return x
  let word0 = alts (map (sequence . map lit) dict)
  (word',word) <- declare"WORD"; produce word' word0 -- It's much slower if we dont make a non-terminal
  fix "words"$ \words -> return$ alts [
    return [],
    do ws <- words; w <- word; return (ws++[w])
    ]

test :: IO Bool
test = do
  contents <- readFile "/usr/share/dict/cracklib-small"
  let dict = ["a","I","on"] ++ filter ((> 2) . length) (Prelude.lines contents)
  run (genLang dict)
  where
    tag = "lost-whitespace"

    input = filter (not . Char.isSpace) $       
      "I saw the man on the hill with a telescope"
    
    expected = Right$ map Prelude.words [
      "I saw the man on the hill with a telescope",
      "I saw them a non the hill with a telescope",
      "I saw them anon the hill with a telescope"
      ]

    run :: Lang Char (Gram [String]) -> IO Bool
    run lang = check (outcome . parseAmb lang) tag input expected
