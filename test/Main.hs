module Main(main) where

import           Example.Arith
import           Example.Catalan
import           Example.DictionaryLexing
import           Example.FunctionalLanguage
import           Example.PrepositionalAttachmentAmbiguity
import           Testing
import           UnitTest

main :: IO ()
main = runAll (concat [
                  UnitTest.tests,
                  Example.Arith.tests,
                  Example.Catalan.tests,
                  Example.FunctionalLanguage.tests,
                  Example.PrepositionalAttachmentAmbiguity.tests,
                  Example.DictionaryLexing.tests, -- takes a few seconds
                  []])
