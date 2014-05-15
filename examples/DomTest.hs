{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module DomTest (main) where

import Prelude
import FFI
import Fay.Text
import Cinder.SVG
import Cinder.SVG.Attributes
import Cinder.SVG.Elements

main :: Fay ()
main = addEventListener "load" dt False

dt :: Fay ()
dt = do
    putStrLn $ show $ pretty mu
    root >>= insert mu
    return ()
    where mu = cRXY 40 100 100 ! fill "blue" ! stroke "yellow"
             !<+ rXYHW 20 20 50 10 ! fill "red"
                !+ aADR "width" 3 4 ! vsN [10,270,300,10] ! Complete
             !< text !: "Some lovely text for the people!" ! xN 100 ! yN 200
                !: " and some more"
             !< Complete

addEventListener :: Text -> Fay () -> Bool -> Fay ()
addEventListener = ffi "window['addEventListener'](%1,%2,%3)"
