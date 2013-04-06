module Bouncer (main) where

import Prelude
import FFI
import Cinder.SVG
import Cinder.SVG.Attributes

main :: Fay ()
main = addEventListener "load" bouncer False

bouncer :: Fay ()
bouncer = do
    root >>= insert (b !+ s !+ rs !+ os !+ su !+ sd)
    return ()
    where
        d = 2.0
        r = 4
        b = markup !+ cRXY 25 50 100 ! fill "cyan" !+ aADR "cy" d r
                !+ bounce 100 400 0.5 3 ! fill "freeze" !< Complete
        s = markup !+ rHWXY 50 50 125 75 ! fill "red" !+ aADR "y" d r
                !+ settle 75 375 0.35 2 ! fill "freeze" !< Complete
        rs = markup !+ rHWXY 50 50 225 75 ! stroke "red" !+ aADR "y" d r
                !+ runningStart 75 375 0.2 ! fill "freeze" !< Complete
        os = markup !+ cRXY 25 350 100 ! stroke "cyan" !+ aADR "cy" d r
                !+ overShoot 100 400 0.2 ! fill "freeze" !< Complete
        su = markup !+ cRXY 25 450 50 ! fill "red" !+ aADR "cy" d r
                !+ speedUp !+ ftN 100 400 ! fill "freeze" !< Complete
        sd = markup !+ rHWXY 50 50 525 75 ! fill "cyan" !+ aADR "y" d r
                !+ slowDown !+ ftN 75 375 ! fill "freeze" !< Complete

addEventListener :: String -> Fay () -> Bool -> Fay ()
addEventListener = ffi "window['addEventListener'](%1,%2,%3)"
