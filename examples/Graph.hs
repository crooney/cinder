module Graph (main) where

import Prelude hiding (id)
import FFI
import Control.Fay
import Cinder.SVG
import Cinder.SVG.Attributes
import System.Random

orig, side :: Double
orig= -200
side = 400

colors :: [String]
colors = ["lightpink","lightblue","lightsalmon","lightgreen","khaki","cyan"]

setup :: Fay ()
setup = do
    vs <- replicateM (length colors) (boundedRand (round side))
    let bs = barPaths vs
    let ps = piePaths vs
    let mu = concat $ zipWith3 go ps bs colors
    grp <- root >>= insert (rc !+ mu)
    ns <- matching "path"
    mapM_ (setListener "mouseover" focus) ns
    mapM_ (setListener "mouseout" unfocus) ns
    insert bg grp
    byId "pBtn" >>= setListener "click" toPie
    byId "bBtn" >>= setListener "click" toBar
  where go p b c = pathD p ! fill c !+ go' "B" p b !<+ go' "P" b p !< Complete
        go' c x y = aADR "d" du 1 ! frz ! classA c ! vs [showD x,showD y]
          !+ beIndef
        bg = pathDR [M orig orig, V side, H side] ! fill "none" ! opacityN 0
            !+ aADR "opacity" du 1 ! classA "B" ! frz !+ ftN 0 0.6 ! Complete
            !+ aADR "opacity" du 1 ! classA "P" ! frz !+ ftN 0.6 0 ! Complete
          !<+ pathDR [M (orig+side+40) (orig+40), L 0 (-20),
            A 20 20 1 1 0 20 20, Z] ! fill "plum" !+ str ! id "pBtn"
          !<+ pathDR [M (orig+side+20) (orig+80), H 10, V 20, H 10, V (-10),
            H 10, V 10, H 10, V 20, H (-40), Z] ! fill "tan" ! id "bBtn"
          ! Complete
        rc = markup ! Element "g" ! transformT [Translate tr tr] !+ str
        tr = negate orig * 1.25
        du = 2.5

piePaths :: [Double] -> [[Seg]]
piePaths xs = zipWith go starts angles
    where go s a = [M c c, L (x s) (y s), L (x s) (y s),
            A r r 0 0 0 (x (s + a)) (y (s + a)), L (x (s + a)) (y (s + a)), Z]
          t = sum xs
          angles = map (\q -> q / t * pi * 2) xs
          starts = scanl (+) 0 angles
          x a = (r * sin a) + c
          y a = (r * cos a) + c
          c = orig + r
          r = side / 2

barPaths :: [Double] -> [[Seg]]
barPaths xs = zipWith go xs [1,3 ..]
    where go x n = [M (s n) gb, L (s n) (t x), L (w + s n) (t x),
                A r r 0 0 0 (w + s n) (t x), L (w + s n) gb, Z]
          t x = gb - x
          s x = (x * w) + orig
          l = fromIntegral $ length xs
          w = side / (l * 2 +1)
          gb = orig + side
          r = side / 2

switch :: String -> Fay ()
-- this is all that's necessary for FF and I think is correct.
-- switch next = matching next >>= mapM_ start
-- full version is to placate Chromium
switch next = do
    ms <- matching next
    ps <- mapM parent ms
    mapM_ deleteSelf ms
    zipWithM_ setParent ps ms
    mapM_ start ms


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
