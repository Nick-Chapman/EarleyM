module Main(main) where

import Testing
import ChartExamples
import NewLangExample
import JuxtaExpExample
import OlegExample

main :: IO ()
main = runAll (concat [
                  ChartExamples.tests,
                  NewLangExample.tests,
                  JuxtaExpExample.tests,
                  OlegExample.tests,
                  []])
