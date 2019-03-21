module Main(main) where

import Testing
import BasicExamples
import JuxtaExpExample
import OlegExample
import EarlyWikiExample
import NlpExample

main :: IO ()
main = runAll (concat [
                  JuxtaExpExample.tests,
                  OlegExample.tests,
                  BasicExamples.tests,
                  EarlyWikiExample.tests,
                  [NlpExample.test],
                  []])
