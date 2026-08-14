module Main (main) where

import qualified CliSpec
import qualified FormulaSpec
import qualified LiveSpec
import qualified SheetFileSpec
import qualified SheetStateSpec
import Test.Tasty (defaultMain, testGroup)
import qualified UpdateSpec

main :: IO ()
main =
  defaultMain $
    testGroup
      "trellis"
      [ SheetStateSpec.tests
      , FormulaSpec.tests
      , UpdateSpec.tests
      , LiveSpec.tests
      , CliSpec.tests
      , SheetFileSpec.tests
      ]
