module Nifty (main) where

import Prelude
import FFI
import Cinder.SVG
import Control.Fay

main :: Fay ()
main = addEventListener "load" nifty False

nifty :: Fay ()
nifty = root >>= byTag "path" >>= mapM (tracePath 0.5 1)
        >>= stagger "begin" 0 0.5 >> return ()

addEventListener :: String -> Fay () -> Bool -> Fay ()
addEventListener = ffi "window['addEventListener'](%1,%2,%3)"
