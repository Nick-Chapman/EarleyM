module Main(main) where

import Testing
import UnitTest
import Example.Arith
import Example.PrepositionalAttachmentAmbiguity
import Example.Catalan
import Example.FunctionalLanguage
import Example.DictionaryLexing

main :: IO ()
main = runAll (concat [
                  UnitTest.tests,
                  Example.Arith.tests,
                  Example.Catalan.tests,
                  Example.FunctionalLanguage.tests,
                  Example.PrepositionalAttachmentAmbiguity.tests,
                  Example.DictionaryLexing.tests, -- takes a few seconds
                  []])
