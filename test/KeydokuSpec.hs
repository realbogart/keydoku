module KeydokuSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Keydoku qualified
import Test.Hspec

spec :: Spec
spec = do
  describe "boardLines" $ do
    it "matches the board template in resources/board.txt" $ do
      template <- readFile "resources/board.txt"
      Keydoku.boardLines `shouldBe` lines template

  describe "selectionKeypadPosition" $ do
    it "maps movement keys to keypad positions" $ do
      Keydoku.selectionKeypadPosition 'u' `shouldBe` Just (Keydoku.KeypadPos 0 0)
      Keydoku.selectionKeypadPosition 'o' `shouldBe` Just (Keydoku.KeypadPos 0 2)
      Keydoku.selectionKeypadPosition '.' `shouldBe` Just (Keydoku.KeypadPos 2 2)

    it "ignores unsupported keys" $ do
      Keydoku.selectionKeypadPosition 'x' `shouldBe` Nothing
      Keydoku.selectionKeypadPosition 'p' `shouldBe` Nothing

  describe "valueKeyToDigit" $ do
    it "maps numpad-like value keys" $ do
      Keydoku.valueKeyToDigit 'm' `shouldBe` Just 1
      Keydoku.valueKeyToDigit 'k' `shouldBe` Just 5
      Keydoku.valueKeyToDigit 'o' `shouldBe` Just 9

    it "ignores non-value keys" $ do
      Keydoku.valueKeyToDigit 'p' `shouldBe` Nothing

  describe "auto candidates" $ do
    it "shows keypad-layout candidates in empty cells" $ do
      Keydoku.candidateAtDisplayCoord Keydoku.initialState 2 1 `shouldBe` Just 7
      Keydoku.candidateAtDisplayCoord Keydoku.initialState 4 1 `shouldBe` Just 8
      Keydoku.candidateAtDisplayCoord Keydoku.initialState 6 1 `shouldBe` Just 9
      Keydoku.candidateAtDisplayCoord Keydoku.initialState 2 2 `shouldBe` Just 4
      Keydoku.candidateAtDisplayCoord Keydoku.initialState 4 2 `shouldBe` Just 5
      Keydoku.candidateAtDisplayCoord Keydoku.initialState 6 2 `shouldBe` Just 6
      Keydoku.candidateAtDisplayCoord Keydoku.initialState 2 3 `shouldBe` Just 1
      Keydoku.candidateAtDisplayCoord Keydoku.initialState 4 3 `shouldBe` Just 2
      Keydoku.candidateAtDisplayCoord Keydoku.initialState 6 3 `shouldBe` Just 3

    it "removes candidates blocked by Sudoku rules" $ do
      let state =
            Keydoku.initialState
              { Keydoku.values =
                  Map.fromList
                    [ (Keydoku.KeypadPos 0 0, 5),
                      (Keydoku.KeypadPos 1 1, 7),
                      (Keydoku.KeypadPos 4 1, 3)
                    ]
              }
      Keydoku.allowedDigitsAt state (Keydoku.KeypadPos 0 1) `shouldNotContain` [5]
      Keydoku.allowedDigitsAt state (Keydoku.KeypadPos 0 1) `shouldNotContain` [7]
      Keydoku.allowedDigitsAt state (Keydoku.KeypadPos 0 1) `shouldNotContain` [3]

  describe "conflictingCells" $ do
    it "marks row/column/box duplicates" $ do
      let state =
            Keydoku.initialState
              { Keydoku.values =
                  Map.fromList
                    [ (Keydoku.KeypadPos 0 0, 5),
                      (Keydoku.KeypadPos 0 2, 5),
                      (Keydoku.KeypadPos 3 0, 5),
                      (Keydoku.KeypadPos 1 1, 5),
                      (Keydoku.KeypadPos 8 8, 9)
                    ]
              }
      Keydoku.conflictingCells state
        `shouldBe` Set.fromList [Keydoku.KeypadPos 0 0, Keydoku.KeypadPos 0 2, Keydoku.KeypadPos 3 0, Keydoku.KeypadPos 1 1]

    it "is empty for non-conflicting boards" $ do
      let state =
            Keydoku.initialState
              { Keydoku.values =
                  Map.fromList
                    [ (Keydoku.KeypadPos 0 0, 1),
                      (Keydoku.KeypadPos 0 1, 2),
                      (Keydoku.KeypadPos 1 3, 1)
                    ]
              }
      Keydoku.conflictingCells state `shouldBe` Set.empty

  describe "statusText" $ do
    it "shows Solved! for a complete, valid board" $ do
      let solvedValues =
            Map.fromList
              [ (Keydoku.KeypadPos row col, ((row * 3 + row `div` 3 + col) `mod` 9) + 1)
                | row <- [0 .. 8],
                  col <- [0 .. 8]
              ]
          state = Keydoku.initialState {Keydoku.values = solvedValues}
      Keydoku.statusText state `shouldBe` "Solved!"

    it "does not show Solved! for incomplete or conflicting boards" $ do
      Keydoku.statusText Keydoku.initialState `shouldBe` "Step 1/3: select quadrant"
      let conflictingState =
            Keydoku.initialState
              { Keydoku.values =
                  Map.fromList
                    [ (Keydoku.KeypadPos 0 0, 1),
                      (Keydoku.KeypadPos 0 1, 1)
                    ]
              }
      Keydoku.statusText conflictingState `shouldBe` "Step 1/3: select quadrant"

  describe "handleKey" $ do
    it "follows quadrant -> cell -> value flow and toggles the digit" $ do
      let state1 = Keydoku.handleKey 'o' Keydoku.initialState
      state1.phase `shouldBe` Keydoku.SelectCell
      state1.selectedQuadrant `shouldBe` Just (Keydoku.KeypadPos 0 2)

      let state2 = Keydoku.handleKey 'k' state1
      state2.phase `shouldBe` Keydoku.SelectValue
      state2.selectedCell `shouldBe` Just (Keydoku.KeypadPos 1 7)

      let state3 = Keydoku.handleKey 'o' state2
      state3.phase `shouldBe` Keydoku.SelectQuadrant
      state3.selectedQuadrant `shouldBe` Nothing
      state3.selectedCell `shouldBe` Nothing
      Keydoku.cellValueAt state3 (Keydoku.KeypadPos 1 7) `shouldBe` Just 9

      let state4 = Keydoku.handleKey 'o' state3
          state5 = Keydoku.handleKey 'k' state4
          state6 = Keydoku.handleKey 'o' state5
      Keydoku.cellValueAt state6 (Keydoku.KeypadPos 1 7) `shouldBe` Nothing

    it "clears selected cell value with n" $ do
      let state1 = Keydoku.handleKey 'o' Keydoku.initialState
          state2 = Keydoku.handleKey 'k' state1
          state3 = Keydoku.handleKey 'o' state2
          state4 = Keydoku.handleKey 'o' state3
          state5 = Keydoku.handleKey 'k' state4
          cleared = Keydoku.handleKey 'n' state5
      Keydoku.cellValueAt cleared (Keydoku.KeypadPos 1 7) `shouldBe` Nothing
      cleared.phase `shouldBe` Keydoku.SelectQuadrant
      cleared.selectedQuadrant `shouldBe` Nothing
      cleared.selectedCell `shouldBe` Nothing

    it "clears selection with h" $ do
      let state1 = Keydoku.handleKey 'o' Keydoku.initialState
          state2 = Keydoku.handleKey 'k' state1
          cleared = Keydoku.handleKey 'h' state2
      cleared.phase `shouldBe` Keydoku.SelectQuadrant
      cleared.selectedQuadrant `shouldBe` Nothing
      cleared.selectedCell `shouldBe` Nothing

    it "undoes board edits with y all the way back to the initial board" $ do
      let state1 = Keydoku.handleKey 'o' Keydoku.initialState
          state2 = Keydoku.handleKey 'k' state1
          state3 = Keydoku.handleKey 'm' state2
          state4 = Keydoku.handleKey 'u' state3
          state5 = Keydoku.handleKey 'k' state4
          state6 = Keydoku.handleKey 'k' state5
          state7 = Keydoku.handleKey 'u' state6
          state8 = Keydoku.handleKey 'k' state7
          state9 = Keydoku.handleKey 'n' state8
          state10 = Keydoku.handleKey 'k' state9
          state11 = Keydoku.handleKey 'k' state10
          state12 = Keydoku.handleKey 'o' state11
          undo1 = Keydoku.handleKey 'y' state12
          undo2 = Keydoku.handleKey 'y' undo1
          undo3 = Keydoku.handleKey 'y' undo2
          undo4 = Keydoku.handleKey 'y' undo3
      Keydoku.cellValueAt undo1 (Keydoku.KeypadPos 4 4) `shouldBe` Nothing
      Keydoku.cellValueAt undo1 (Keydoku.KeypadPos 1 1) `shouldBe` Nothing
      Keydoku.cellValueAt undo1 (Keydoku.KeypadPos 1 7) `shouldBe` Just 1
      Keydoku.cellValueAt undo2 (Keydoku.KeypadPos 1 1) `shouldBe` Just 5
      Keydoku.cellValueAt undo2 (Keydoku.KeypadPos 1 7) `shouldBe` Just 1
      Keydoku.cellValueAt undo3 (Keydoku.KeypadPos 1 1) `shouldBe` Nothing
      Keydoku.cellValueAt undo3 (Keydoku.KeypadPos 1 7) `shouldBe` Just 1
      undo4.values `shouldBe` Keydoku.initialState.values
      undo4.phase `shouldBe` Keydoku.SelectQuadrant
      undo4.selectedQuadrant `shouldBe` Nothing
      undo4.selectedCell `shouldBe` Nothing

    it "ignores undo when no move has been made" $ do
      Keydoku.handleKey 'y' Keydoku.initialState `shouldBe` Keydoku.initialState

    it "redoes board edits with p after undo" $ do
      let state1 = Keydoku.handleKey 'o' Keydoku.initialState
          state2 = Keydoku.handleKey 'k' state1
          state3 = Keydoku.handleKey 'm' state2
          undone = Keydoku.handleKey 'y' state3
          redone = Keydoku.handleKey 'p' undone
      Keydoku.cellValueAt undone (Keydoku.KeypadPos 1 7) `shouldBe` Nothing
      Keydoku.cellValueAt redone (Keydoku.KeypadPos 1 7) `shouldBe` Just 1

    it "clears redo history after a new edit" $ do
      let state1 = Keydoku.handleKey 'o' Keydoku.initialState
          state2 = Keydoku.handleKey 'k' state1
          state3 = Keydoku.handleKey 'm' state2
          undone = Keydoku.handleKey 'y' state3
          state4 = Keydoku.handleKey 'u' undone
          state5 = Keydoku.handleKey 'k' state4
          changed = Keydoku.handleKey 'k' state5
          redoIgnored = Keydoku.handleKey 'p' changed
      Keydoku.cellValueAt changed (Keydoku.KeypadPos 1 1) `shouldBe` Just 5
      redoIgnored `shouldBe` changed

  describe "fixed cells" $ do
    it "does not clear a fixed cell with n" $ do
      let cell = Keydoku.KeypadPos 0 0
          state =
            Keydoku.initialState
              { Keydoku.phase = Keydoku.SelectValue,
                Keydoku.selectedCell = Just cell,
                Keydoku.values = Map.fromList [(cell, 7)],
                Keydoku.fixedCells = Set.fromList [cell]
              }
          cleared = Keydoku.handleKey 'n' state
      Keydoku.cellValueAt cleared cell `shouldBe` Just 7

    it "does not overwrite a fixed cell value" $ do
      let cell = Keydoku.KeypadPos 0 0
          state =
            Keydoku.initialState
              { Keydoku.phase = Keydoku.SelectValue,
                Keydoku.selectedCell = Just cell,
                Keydoku.values = Map.fromList [(cell, 7)],
                Keydoku.fixedCells = Set.fromList [cell]
              }
          updated = Keydoku.handleKey 'm' state
      Keydoku.cellValueAt updated cell `shouldBe` Just 7

  describe "parseClipboardBoard" $ do
    it "parses a valid 9x9 clipboard board with spaces as blanks" $ do
      let boardText =
            unlines
              [ "1   34  8",
                " 7 68  3 ",
                "  821 7 4",
                " 54 9 68 ",
                "91 5 8 2 ",
                " 8 3    5",
                "3 59 6871",
                "  6    4 ",
                "  1 7 2  "
              ]
      case Keydoku.parseClipboardBoard boardText of
        Left err -> expectationFailure ("expected successful parse, got: " ++ err)
        Right values -> do
          Map.lookup (Keydoku.KeypadPos 0 0) values `shouldBe` Just 1
          Map.lookup (Keydoku.KeypadPos 0 1) values `shouldBe` Nothing
          Map.lookup (Keydoku.KeypadPos 0 4) values `shouldBe` Just 3
          Map.lookup (Keydoku.KeypadPos 8 2) values `shouldBe` Just 1

    it "rejects invalid clipboard boards" $ do
      Keydoku.parseClipboardBoard "123" `shouldBe` Left "expected exactly 9 lines"
      Keydoku.parseClipboardBoard (unlines (replicate 9 "12345678")) `shouldBe` Left "each line must contain exactly 9 characters"
      Keydoku.parseClipboardBoard (unlines (replicate 9 "12345678x"))
        `shouldBe` Left "invalid character at row 1, col 9 (use digits 1-9 or spaces)"
