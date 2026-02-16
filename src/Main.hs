module Main where

import Keydoku qualified
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> Keydoku.main
    [boardFile] -> Keydoku.mainWithBoardFile (Just boardFile)
    _ -> do
      putStrLn "Usage: keydoku [BOARD_FILE]"
      exitFailure
