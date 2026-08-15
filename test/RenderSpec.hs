module RenderSpec (tests) where

import Formula (Value (..))
import Render.Theme (
  boolBg,
  boolFg,
  errBg,
  errFg,
  numBg,
  numFg,
  strBg,
  strFg,
  textBg,
  textFg,
  valueColors,
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@=?))

tests :: TestTree
tests =
  testGroup
    "Render.Theme"
    [ testCase "VBlank maps to default text colors" $
        valueColors VBlank @=? (textFg, textBg)
    , testCase "VNum maps to orange" $
        valueColors (VNum 42) @=? (numFg, numBg)
    , testCase "VStr maps to header treatment" $
        valueColors (VStr "hello") @=? (strFg, strBg)
    , testCase "VBool maps to green" $
        valueColors (VBool True) @=? (boolFg, boolBg)
    , testCase "VErr maps to red" $
        valueColors (VErr "boom") @=? (errFg, errBg)
    , testCase "strBg differs from textBg (header contrast)" $
        assertBool "strBg should differ from textBg" (strBg /= textBg)
    , testCase "numFg differs from textFg (type distinction)" $
        assertBool "numFg should differ from textFg" (numFg /= textFg)
    , testCase "errFg differs from boolFg (error vs boolean)" $
        assertBool "errFg should differ from boolFg" (errFg /= boolFg)
    ]
