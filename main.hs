module Main(main) where

import Testing
import BasicExamples
import JuxtaExpExample
import OlegExample

main :: IO ()
main = runAll (concat [
                  BasicExamples.tests,
                  JuxtaExpExample.tests,
                  OlegExample.tests,
                  []])
