module Main where

import Criterion.Main
import Keydoku qualified

-- Example benchmarks for common operations
main :: IO ()
main =
  defaultMain
    [ bgroup
        "String operations"
        [ bench "reverse short string" $ whnf reverse "hello",
          bench "concatenation" $ whnf (++ "world") "hello "
        ]
    ]
