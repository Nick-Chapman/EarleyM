module Main(main) where

import Testing
import NewLangExample
import JuxtaExpExample
import OlegExample

main :: IO ()
main = runAll (concat [
                  NewLangExample.tests,
                  JuxtaExpExample.tests,
                  OlegExample.tests,
                  []])
