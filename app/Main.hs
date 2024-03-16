module Main where

import Criterion.Main
import P023 qualified as P023

main :: IO ()
main =
  -- print P023.p023
  defaultMain
    [ bgroup
        "Project Euler solutions"
        [ P023.p023Benchmark ]
    ]
