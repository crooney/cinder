{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Nifty (main) where

import Prelude
import FFI
import Fay.Text
import Cinder.SVG

main :: Fay ()
main = addEventListener "load" nifty False

nifty :: Fay ()
nifty = root >>= byTag "path" >>= mapM (tracePath 1 1)
        >>= stagger "begin" 0 1 >> return ()

addEventListener :: Text -> Fay () -> Bool -> Fay ()
addEventListener = ffi "window['addEventListener'](%1,%2,%3)"
