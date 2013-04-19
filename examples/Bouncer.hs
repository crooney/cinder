module Bouncer (main) where

import Prelude
import FFI
import Control.Fay
import Cinder.SVG
import Cinder.SVG.Attributes

bouncer :: Fay ()
bouncer = do
    g <- node "g" >>= insert circ
    byTag "circle" g >>= stagger "cx" 50 100
    byClass "drop" g >>= zipInsert fxd >>= stagger "begin" 0 2
    byClass "grow" g >>= zipInsert fxg >>= stagger "begin" 9 0.75
    root >>= setChild g
    return ()
    where
        circ = foldl1 (!+) $ map go colors
        go x = cRXY 25 50 100 ! fill x
                    !+ aADR "cy" 2.0 1 ! fill "freeze" ! classA "drop"
                    !<+ aADR "r" 0.75 1 ! fill "freeze" ! classA "grow"
               !< Complete
        fxd = [bounce 0.4 3 100 400, settle 0.35 3 100 400,
               bounce 0.75 4 100 400, settle 0.65 4 100 400]
        fxg = [bounce 0.5 5 25 50, settle 0.45 5 25 50,
               bounce 0.85 6 25 50, settle 0.75 6 25 50]
        colors = ["lightpink", "lightblue", "lightsalmon", "lightgreen"]

main :: Fay ()
main = addEventListener "load" bouncer False

addEventListener :: String -> Fay () -> Bool -> Fay ()
addEventListener = ffi "window['addEventListener'](%1,%2,%3)"
