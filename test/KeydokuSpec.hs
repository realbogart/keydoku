module KeydokuSpec (spec) where

import Keydoku qualified
import Test.Hspec

spec :: Spec
spec = do
  describe "boardLines" $ do
    it "matches the board template in resources/board.txt" $ do
      template <- readFile "resources/board.txt"
      Keydoku.boardLines `shouldBe` lines template

  describe "keypadPosition" $ do
    it "maps movement keys to keypad positions" $ do
      Keydoku.keypadPosition 'u' `shouldBe` Just (Keydoku.KeypadPos 0 0)
      Keydoku.keypadPosition 'k' `shouldBe` Just (Keydoku.KeypadPos 1 1)
      Keydoku.keypadPosition '.' `shouldBe` Just (Keydoku.KeypadPos 2 2)

    it "ignores unsupported keys" $ do
      Keydoku.keypadPosition 'x' `shouldBe` Nothing

  describe "advanceSelection" $ do
    it "first key selects quadrant" $ do
      let state = Keydoku.advanceSelection (Keydoku.KeypadPos 0 2) Keydoku.initialState
      state.phase `shouldBe` Keydoku.SelectCell
      state.selectedQuadrant `shouldBe` Just (Keydoku.KeypadPos 0 2)
      state.selectedCell `shouldBe` Nothing

    it "second key selects absolute cell and returns to quadrant mode" $ do
      let withQuadrant = Keydoku.advanceSelection (Keydoku.KeypadPos 2 0) Keydoku.initialState
          withCell = Keydoku.advanceSelection (Keydoku.KeypadPos 1 2) withQuadrant
      withCell.phase `shouldBe` Keydoku.SelectQuadrant
      withCell.selectedQuadrant `shouldBe` Just (Keydoku.KeypadPos 2 0)
      withCell.selectedCell `shouldBe` Just (Keydoku.KeypadPos 7 2)
