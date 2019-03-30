module Example.DictionaryLexing(tests) where

import Prelude hiding(fail)
import qualified Data.Char as Char
import Testing
import EarleyM

-- This example constructs a very large grammar (from a dictionary), which is used to lex a sentence.
-- The sentence has had it's interword whitespace removed.

-- The parser recovers the original words/sentence.
-- As well as finding some alternative sentences, which look the same when spaces are removed.

genLang :: [String] -> Lang Char (Gram [String])
genLang dict = do
  token <- getToken
  let symbol x = do t <-token; if t==x then return () else fail
  let lit x = do symbol x; return x
  let word0 = alts (map (sequence . map lit) dict)
  (word',word) <- declare"WORD"; produce word' word0 -- It's much slower if we dont make a non-terminal
  fix "words"$ \words -> return$ alts [
    return [],
    do ws <- words; w <- word; return (ws++[w])
    ]

test1 :: IO Bool
test1 = do
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


tests :: [IO Bool]
tests = [test1]
