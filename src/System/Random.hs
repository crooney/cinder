module System.Random
    (
    boundedRand
    ,rand
    ) where

import Prelude
import FFI

boundedRand :: Int -> Fay Double
boundedRand = ffi "Math.floor(%1 * Math.random())"

rand :: Fay Double
rand = ffi "Math.random()"
