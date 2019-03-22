module LostWhitespaceExample(test) where

import Prelude hiding (fail,words)
import qualified Data.Char as Char
import Testing
import Chart

-- This is an example of using a dictionary to lex a sentence back into words,
-- after having had the interword space removed!

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

    input = "I saw the man on the hill with a telescope"
    expectedAmbCount = 3
                       -- I saw the man on the hill with a telescope
                       -- I saw them a non the hill with a telescope
                       -- I saw them anon the hill with a telescope
    
    chars = filter (not . Char.isSpace) input
    run lang = runL chars expectedAmbCount
      where
        (runL,_) = runTestCountAmbiguity tag lang
