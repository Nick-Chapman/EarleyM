module Main(main) where

import Testing
import BasicExamples
import JuxtaExpExample
import OlegExample

main :: IO ()
main = runAll (concat [
                  JuxtaExpExample.tests,
                  OlegExample.tests,
                  BasicExamples.tests,
                  []])
