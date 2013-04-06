module Bouncer (main) where

import Prelude
import FFI
import Cinder.SVG
import Cinder.SVG.Attributes

bouncer :: Fay ()
bouncer = do
    root >>= insert circ
    root >>= byTag "circle" >>= stagger "cx" 50 100
    root >>= byTag "animate" >>= zipInsert fx >>= stagger "begin" 0 2
    return ()
    where
        circ = foldl1 (!+) $ map go colors
        go x = cRXY 25 50 100 ! fill x !+ aADR "cy" 2.0 1 ! fill "freeze"
                    ! begin "indefinite" !< Complete
        fx = [bounce 100 400 0.4 3, settle 100 400 0.35 3,
              bounce 100 400 0.75 4, settle 100 400 0.65 4]
        colors = ["lightpink", "lightblue", "lightsalmon", "lightgreen"]

main :: Fay ()
main = addEventListener "load" bouncer False

addEventListener :: String -> Fay () -> Bool -> Fay ()
addEventListener = ffi "window['addEventListener'](%1,%2,%3)"
