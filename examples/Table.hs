module Table (main) where

import Prelude hiding (id)
import FFI
import Control.Fay
import Cinder.HTML
import Cinder.HTML.Elements hiding (map,style)
import Cinder.HTML.Attributes

colors = go [ (r,g,b) | r <- s, g <- s, b <- s]
    where s = [0, 0x33 .. 0xff]
          go [] = []
          go xs = map rgb (take 24 xs) : go (drop 24 xs)

tableMU = markup ! table !+ foldl1 (!+) (map row colors) ! Complete
    where row x = markup ! tr !+ foldl1 (!+) (map box x) ! Complete
          box x = markup ! td ! sty x ! inner "&#955;" ! Complete
          sty x = style ("height:20px;width:20px;background-color:" ++ x)

htex :: Fay ()
htex = root >>= byTag "body" >>= zipWithM_ insert [tableMU]

main :: Fay ()
main = addEventListener "load" htex False

addEventListener :: String -> Fay () -> Bool -> Fay ()
addEventListener = ffi "window['addEventListener'](%1,%2,%3)"
