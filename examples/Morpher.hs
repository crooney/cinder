module Morpher (main) where

import Prelude
import FFI
import Cinder.SVG
import Cinder.SVG.Attributes

main :: Fay ()
main = addEventListener "load" morpher False

morpher :: Fay ()
morpher = do
    root >>= insert mu
    return ()
    where
        mu = markup !+ pathD dFrom ! fill "cyan" !+ aADR "d" 5.5 9
            ! vs [showD dFrom,showD dTo,showD dFrom] !< Complete
        dFrom = [M 200 200, Q 510 10 320 200, Q 510 510 320 320,
                Q 10 510 200 320, Q 10 10 200 200]
        dTo = [M 10 10, Q 125 75 190 10, Q 125 125 190 190,
                Q 75 125 10 190, Q 75 75 10 10]

addEventListener :: String -> Fay () -> Bool -> Fay ()
addEventListener = ffi "window['addEventListener'](%1,%2,%3)"
