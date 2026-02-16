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
