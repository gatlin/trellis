module SheetFileSpec (tests) where

import qualified Data.Map.Strict as Map
import Formula (blank)
import Parser (parseExpr)
import SheetFile (FileEntry (..), parseSheetFile, serialize)
import SheetState (LiveBinding (..), SheetState (..), initialState)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Trellis.Orc as Orc

tests :: TestTree
tests =
  testGroup
    "SheetFile"
    [ parseSheetFileTests
    , serializeTests
    ]

parseSheetFileTests :: TestTree
parseSheetFileTests =
  testGroup
    "parseSheetFile"
    [ testCase "a cell line" $
        parseSheetFile "0,0=10" @?= ([], [CellEntry (0, 0) "10"])
    , testCase "an OUT line" $
        parseSheetFile "OUT 3,0=/tmp/status.fifo"
          @?= ([], [OutEntry (3, 0) "/tmp/status.fifo"])
    , testCase "blank lines and # comments are skipped" $
        parseSheetFile "# a sheet\n\n0,0=1\n\n# trailing\n1,0=2"
          @?= ([], [CellEntry (0, 0) "1", CellEntry (1, 0) "2"])
    , testCase "a malformed line warns and is skipped, the rest still loads" $
        parseSheetFile "0,0=1\nnot a line\n1,0=2"
          @?= ( ["line 2: malformed: not a line"]
              , [CellEntry (0, 0) "1", CellEntry (1, 0) "2"]
              )
    , testCase "a formula containing its own '=' round-trips" $
        parseSheetFile "0,0=1=1" @?= ([], [CellEntry (0, 0) "1=1"])
    , testCase "a live spec is kept as its own text, undecided here" $
        parseSheetFile "2,0=!5s date" @?= ([], [CellEntry (2, 0) "!5s date"])
    ]

serializeTests :: TestTree
serializeTests =
  testGroup
    "serialize"
    [ testCase "populated cells and out-bindings all appear" $ do
        let Right ten = parseExpr "10"
            Right eleven = parseExpr "@0,0+1"
            st =
              initialState
                { cells = Map.fromList [((0, 0), ten), ((1, 0), eleven)]
                , outBindings = Map.fromList [((3, 0), "/tmp/status.fifo")]
                }
        serialize st
          @?= unlines
            [ "0,0=10"
            , "1,0=@0,0+1"
            , "OUT 3,0=/tmp/status.fifo"
            ]
    , testCase "a live cell's spec text is written verbatim, not its value" $ do
        grp <- Orc.newRootGroup
        let st =
              initialState
                { subscriptions = Map.fromList [((2, 0), LiveBinding grp "!5s date")]
                }
        serialize st @?= "2,0=!5s date\n"
    , testCase "a cell with no formula and no subscription serializes as blank" $
        serialize (initialState{cells = Map.fromList [((0, 0), blank)]})
          @?= "0,0=\n"
    , testCase "serialize then parseSheetFile reconstructs the same entries" $ do
        let Right ten = parseExpr "10"
            st =
              initialState
                { cells = Map.fromList [((0, 0), ten)]
                , outBindings = Map.fromList [((3, 0), "/tmp/out")]
                }
            (warnings, entries) = parseSheetFile (serialize st)
        warnings @?= []
        entries @?= [CellEntry (0, 0) "10", OutEntry (3, 0) "/tmp/out"]
    ]
