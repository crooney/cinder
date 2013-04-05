module Bouncer (main) where

import Prelude
import FFI
import Cinder.SVG
import Cinder.SVG.Attributes

main :: Fay ()
main = addEventListener "load" bouncer False

bouncer :: Fay ()
bouncer = do
    root >>= insert b
    return ()
    where
        b = markup !+ cRXY 25 50 50 ! fill "cyan" !+ aADR "cy" 3 0 ! beginN 0
                !+ bounce 50 400 0.5 2 !< Complete

addEventListener :: String -> Fay () -> Bool -> Fay ()
addEventListener = ffi "window['addEventListener'](%1,%2,%3)"
