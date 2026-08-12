module Main (main) where

import qualified FormulaSpec
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
      ]
