module Main where

import KeydokuSpec qualified
import Test.Hspec

main :: IO ()
main = hspec KeydokuSpec.spec
