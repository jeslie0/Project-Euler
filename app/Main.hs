module Main where

import Criterion.Main
import P022 qualified as P022

main :: IO ()
main =
  defaultMain
    [ bgroup
        "Project Euler solutions"
        [ P022.p022Benchmark ]
    ]
