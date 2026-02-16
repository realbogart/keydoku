module Main where

import Test.Hspec
import KeydokuSpec qualified

main :: IO ()
main = hspec KeydokuSpec.spec