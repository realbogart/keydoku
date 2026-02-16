module KeydokuSpec (spec) where

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
      Keydoku.selectionKeypadPosition 'p' `shouldBe` Just (Keydoku.KeypadPos 0 2)
      Keydoku.selectionKeypadPosition '.' `shouldBe` Just (Keydoku.KeypadPos 2 2)

    it "ignores unsupported keys" $ do
      Keydoku.selectionKeypadPosition 'x' `shouldBe` Nothing
      Keydoku.selectionKeypadPosition 'o' `shouldBe` Nothing

  describe "valueKeyToDigit" $ do
    it "maps numpad-like value keys" $ do
      Keydoku.valueKeyToDigit 'm' `shouldBe` Just 1
      Keydoku.valueKeyToDigit 'k' `shouldBe` Just 5
      Keydoku.valueKeyToDigit 'o' `shouldBe` Just 9

    it "ignores non-value keys" $ do
      Keydoku.valueKeyToDigit 'p' `shouldBe` Nothing

  describe "handleKey" $ do
    it "follows quadrant -> cell -> value flow and toggles the digit" $ do
      let state1 = Keydoku.handleKey 'p' Keydoku.initialState
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

      let state4 = Keydoku.handleKey 'p' state3
          state5 = Keydoku.handleKey 'k' state4
          state6 = Keydoku.handleKey 'o' state5
      Keydoku.cellValueAt state6 (Keydoku.KeypadPos 1 7) `shouldBe` Nothing

    it "clears selected cell value with n" $ do
      let state1 = Keydoku.handleKey 'p' Keydoku.initialState
          state2 = Keydoku.handleKey 'k' state1
          state3 = Keydoku.handleKey 'o' state2
          state4 = Keydoku.handleKey 'p' state3
          state5 = Keydoku.handleKey 'k' state4
          cleared = Keydoku.handleKey 'n' state5
      Keydoku.cellValueAt cleared (Keydoku.KeypadPos 1 7) `shouldBe` Nothing
      cleared.phase `shouldBe` Keydoku.SelectQuadrant
      cleared.selectedQuadrant `shouldBe` Nothing
      cleared.selectedCell `shouldBe` Nothing

    it "clears selection with h" $ do
      let state1 = Keydoku.handleKey 'p' Keydoku.initialState
          state2 = Keydoku.handleKey 'k' state1
          cleared = Keydoku.handleKey 'h' state2
      cleared.phase `shouldBe` Keydoku.SelectQuadrant
      cleared.selectedQuadrant `shouldBe` Nothing
      cleared.selectedCell `shouldBe` Nothing
