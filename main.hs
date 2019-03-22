module Main(main) where

import Testing
import BasicExamples
import JuxtaExpExample
import OlegExample
import EarlyWikiExample
import NlpExample
--import LostWhitespaceExample

main :: IO ()
main = runAll (concat [
                  OlegExample.tests,
                  BasicExamples.tests,
                  EarlyWikiExample.tests,
                  JuxtaExpExample.tests,
                  NlpExample.tests,
                  --[LostWhitespaceExample.test], -- takes a few seconds
                  []])
