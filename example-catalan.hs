module Example.Catalan(tests) where

import Testing
import Earley


tests :: [IO Bool]
tests = [
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
    run = check (countAmb . outcome . parseAmb lang) tag
      where
        countAmb :: Either e [a] -> Int
        countAmb (Left _) = 0
        countAmb (Right xs) = length xs
    
    seq g1 g2 = do x1 <- g1; x2 <- g2; return$ "("++x1++x2++")"
    
    lang = do
      let x = do c <- token; return [c]
      fix"cat" $ \c -> return $ alts [
        seq x x,
        seq x c,
        seq c x,
        seq c c
        ]
