module Main (main) where

import qualified CliSpec
import qualified FormulaSpec
import qualified KeymapSpec
import qualified LiveSpec
import qualified RenderHelpSpec
import qualified RenderSpec
import qualified SheetFileSpec
import qualified SheetStateSpec
import Test.Tasty (defaultMain, testGroup)
import qualified UpdateNavigationSpec
import qualified UpdateSpec

main :: IO ()
main =
  defaultMain $
    testGroup
      "trellis"
      [ SheetStateSpec.tests
      , FormulaSpec.tests
      , UpdateNavigationSpec.tests
      , UpdateSpec.tests
      , LiveSpec.tests
      , CliSpec.tests
      , SheetFileSpec.tests
      , RenderSpec.tests
      , RenderHelpSpec.tests
      , KeymapSpec.tests
      ]
