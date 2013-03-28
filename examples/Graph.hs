module Graph (main) where

import Prelude hiding (id)
import FFI
import Control.Fay
import Cinder.SVG
import Cinder.SVG.Attributes
import System.Random

top, tall, left, wide :: Double
top = -200
tall = 400
left = top
wide = tall

colors :: [String]
colors = ["lightpink","lightblue","lightsalmon","lightgreen","khaki","cyan"]

setup :: Fay ()
setup = do
    vs <- replicateM (length colors) (boundedRand (round tall))
    let bs = barPaths vs
    let ps = piePaths vs
    let mu = concat $ zipWith3 go ps bs colors
    grp <- root >>= insert (rc !+ mu)
    ns <- matching "path"
    mapM_ (setListener "mouseover" focus) ns
    mapM_ (setListener "mouseout" unfocus) ns
    insert bg grp
    byId "PBtn" >>= setListener "click" toPie
    byId "BBtn" >>= setListener "click" toBar
  where go p b c = pathD p ! fill c !+ str
          !+ go' "B" p b !<+ go' "P" b p !< Complete
        go' c x y = aADR "d" du 1 ! frz ! classA c ! vs [showD x,showD y]
        bg = markup -- ! viewBox "-250 -250 250 250"
          !+ pathDR [M left top, V tall, H wide] ! fill "none"
            !+ str ! opacityN 0
            !+ aADR "opacity" du 1 ! classA "B" ! frz !+ ftN 0 0.6 ! Complete
            !+ aADR "opacity" du 1 ! classA "P" ! frz !+ ftN 0.6 0 ! Complete
          !<+ pathDR [M (left + wide + 40) (top + 40), L 0 (-20),
            A 20 20 1 1 0 20 20, Z] ! fill "plum" !+ str ! id "PBtn"
          !<+ pathDR [M (left + wide + 20) (top + 80), H 10, V 20, H 10, V (-10),
            H 10, V 10, H 10, V 20, H (-40), Z] ! fill "tan" !+ str ! id "BBtn"
          ! Complete
        rc = markup ! Element "g" ! id "recenter"  
          ! transformT [Translate (negate top * 1.25) (negate left * 1.25)]
        du = 2.5

piePaths :: [Double] -> [[Seg]]
piePaths xs = zipWith go starts angles
    where go s a = [M cx cy, L (x s) (y s), L (x s) (y s),
            A r r 0 0 0 (x (s + a)) (y (s + a)), L (x (s + a)) (y (s + a)), Z]
          t = sum xs
          angles = map (\q -> q / t * pi * 2) xs
          starts = scanl (+) 0 angles
          x a = (r * sin a) + cx
          y a = (r * cos a) + cy
          cx = left + r
          cy = top + r
          r = tall / 2

barPaths :: [Double] -> [[Seg]]
barPaths xs = zipWith go xs [1,3 ..]
    where go x n = [M (s n) gb, L (s n) (t x), L (w + s n) (t x),
                A r r 0 0 0 (w + s n) (t x), L (w + s n) gb, Z]
          t x = gb - x
          s x = (x * w) + left
          l = fromIntegral $ length xs
          w = wide / (l * 2 +1)
          gb = top + tall
          r = tall / 2

switch :: String -> Fay ()
switch next = matching next >>= mapM_ start

toPie,toBar :: Event -> Fay ()
toPie _ = switch "P"
toBar _ = switch "B"

str :: Markup
str = markup ! stroke "white" ! strokeWidth "4"

frz :: Primitive
frz = fill "freeze"

data Event

focus :: Event -> Fay ()
focus e = do
    tgt e >>= insert (aADR "opacity" 1.5 0 ! classA "transient"
        ! vsN [0.9, 0.5, 0.9])
    return ()

unfocus :: Event -> Fay ()
unfocus _ = matching "transient" >>= mapM_ deleteSelf

tgt :: Event -> Fay Node
tgt = ffi "%1['target']"

matching :: String -> Fay [Node]
matching = ffi "document['getElementsByClassName'](%1)"

setListener :: String -> (Event -> Fay ()) -> Node -> Fay ()
setListener = ffi "%3['addEventListener'](%1,%2,null)"

addEventListener :: String -> Fay () -> Bool -> Fay ()
addEventListener = ffi "window['addEventListener'](%1,%2,%3)"

main :: Fay ()
main = addEventListener "load" setup False
