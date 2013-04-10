module Bouncer (main) where

import Prelude
import FFI
import Control.Fay
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
        fx = [bounce 0.4 3 100 400, settle 0.35 3 100 400,
              bounce 0.75 4 100 400, settle 0.65 4 100 400]
        colors = ["lightpink", "lightblue", "lightsalmon", "lightgreen"]

grower :: Fay ()
grower = do
            root >>= byTag "animate" >>= mapM_ deleteSelf
            root >>= byTag "circle" >>= zipInsert an
            root >>= byTag "animate" >>= stagger "begin" 9 0.75
            return ()
    where
        an = zipWith (!+) (repeat (aADR "r" 0.75 1 ! fill "freeze"))
                            [bounce 0.5 5 25 50, settle 0.45 5 25 50,
                             bounce 0.85 4 25 50, settle 0.75 4 25 50]

main :: Fay ()
main = addEventListener "load" bouncer False
        >> startLater grower 8500

startLater :: Fay () -> Double -> Fay ()
startLater = ffi "setTimeout(%1,%2)"

addEventListener :: String -> Fay () -> Bool -> Fay ()
addEventListener = ffi "window['addEventListener'](%1,%2,%3)"
