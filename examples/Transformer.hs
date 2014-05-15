{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Transformer (main) where

import Prelude
import FFI
import Fay.Text (Text, fromString)
import Cinder.SVG
import Cinder.SVG.Attributes

main :: Fay ()
main = addEventListener "load" transformer False

transformer :: Fay ()
transformer = do
    root >>= insert (markup !+ rHWXY 10 15 10 10 ! fill "green"
        !+ atDRT 4 7 [Scale2d 1 1, Scale2d 2 3, Scale2d 1 1] ! additive "sum"
        !<+ atDRT 3 8 [Rotate 0 100 100,Rotate 270 100 100,Rotate 0 100 100]
            ! additive "sum"
        !< Complete)
    return ()

addEventListener :: Text -> Fay () -> Bool -> Fay ()
addEventListener = ffi "window['addEventListener'](%1,%2,%3)"
