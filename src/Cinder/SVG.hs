module Cinder.SVG
    (module Cinder.SVG, module Cinder.DOM, module Cinder.DSL)
    where

import Prelude hiding (min, max)
import FFI
import Cinder.DSL
import Cinder.DOM
import Cinder.SVG.Attributes
import Cinder.SVG.Elements

xmlns :: String
xmlns = "http://www.w3.org/2000/svg"

cR :: a -> Markup
cR r = markup ! circle ! rN r

cXYR :: a -> a -> a -> Markup
cXYR x y r = cRXY r x y

cRXY :: a -> a -> a -> Markup
cRXY r x y = cR r ! cxN x ! cyN y

rHW :: a -> a -> Markup
rHW h w = markup ! rect ! heightN h ! widthN w

rHWXY :: a -> a -> a -> a -> Markup
rHWXY h w x y = rHW h w ! xN x ! yN y

rXYHW :: a -> a -> a -> a -> Markup
rXYHW x y h w = rHWXY h w x y

eRR :: a -> a -> Markup
eRR rx ry = markup ! ellipse ! rxN rx ! ryN ry

eRRXY :: a -> a -> a -> a -> Markup
eRRXY rx ry x y = eRR rx ry ! cxN x ! cyN y

lXYXY :: a -> a -> a -> a -> Markup
lXYXY x y x' y' = markup ! line ! x1N x ! y1N y ! x2N x' ! y2N y'

lXXYY :: a -> a -> a -> a -> Markup
-- I know it looks stupid w/o y' but hlint wants I should eta reduce
lXXYY x x' y = lXYXY x y x'

data Seg = M Double Double
         | L Double Double
         | H Double
         | V Double
         | A Double Double Double Double Double Double Double
         | C Double Double Double Double Double Double
         | S Double Double Double Double
         | Q Double Double Double Double
         | T Double Double
         | Z
    deriving Show

showD :: [Seg] -> String
showD = concatMap hShow

showDRelative :: [Seg] -> String
showDRelative = toLower . showD

dS :: [Seg] -> Primitive
dS = d . showD

dSR :: [Seg] -> Primitive
dSR = d . showDRelative

setATB :: String -> String -> a -> Markup
setATB a t b = markup ! set ! attributeName a ! to t ! beginN b

animationADR :: String -> a -> Double -> Markup
animationADR a d r = markup ! attributeName a ! durN d ! repeatCount (go r)
                     where go x | x > 0 = show x
                           go _         = "indefinite"

aADR :: String -> a -> Double -> Markup
aADR a d r = markup ! animate !+ animationADR a d r

beIndef :: Markup
beIndef = markup ! begin "indefinite" ! end "indefinite"

ft :: a -> a -> Markup
ft f t = markup ! fromN f ! toN t

vs :: [String] -> Primitive
vs = values . intercalate ";"

vsN :: [a] -> Primitive
vsN = vs . map show

pc :: a -> String
pc x = show x ++ "%"

--non-pure SVG specific stuff (animations and namespaces, mostly)
insert :: Markup -> Node -> Fay Node
insert = insertNS xmlns

node :: DString -> Fay Node
node = nodeNS xmlns

start :: Node -> Fay Node
start = ffi "%1['beginElement']() || %1"

stop :: Node -> Fay Node
stop = ffi "%1['endElement']() || %1"

-- HACK!!!
inst :: Automatic a -> String
inst = ffi "%1['instance'] + ' '"

slot :: Int -> Automatic a -> String
slot = ffi "(%2['slot'+%1] != null && (%2['slot'+%1] + ' ')) || ''"

hShow :: Automatic a -> String
hShow x = inst x ++ concatMap (`slot` x) [1 .. 8]

-- Simple combinators for conversion to string.  Nothing interesting
-- follows.
beginN :: a -> Primitive
beginN x = begin (show x)

cxN :: a -> Primitive
cxN x = cx (show x)

cyN :: a -> Primitive
cyN x = cy (show x)

divisorN :: a -> Primitive
divisorN x = divisor (show x)

durN :: a -> Primitive
durN x = dur (show x)

dxN :: a -> Primitive
dxN x = dx (show x)

dyN :: a -> Primitive
dyN x = dy (show x)

elevationN :: a -> Primitive
elevationN x = elevation (show x)

endN :: a -> Primitive
endN x = end (show x)

exponentN :: a -> Primitive
exponentN x = exponent (show x)

fromN :: a -> Primitive
fromN x = from (show x)

fxN :: a -> Primitive
fxN x = fx (show x)

fyN :: a -> Primitive
fyN x = fy (show x)

heightN :: a -> Primitive
heightN x = height (show x)

interceptN :: a -> Primitive
interceptN x = intercept (show x)

kN :: a -> Primitive
kN x = k (show x)

lengthAdjustN :: a -> Primitive
lengthAdjustN x = lengthAdjust (show x)

maxN :: a -> Primitive
maxN x = max (show x)

minN :: a -> Primitive
minN x = min (show x)

originN :: a -> Primitive
originN x = origin (show x)

pathLengthN :: a -> Primitive
pathLengthN x = pathLength (show x)

primitiveUnitsN :: a -> Primitive
primitiveUnitsN x = primitiveUnits (show x)

rN :: a -> Primitive
rN x = r (show x)

radiusN :: a -> Primitive
radiusN x = radius (show x)

refXN :: a -> Primitive
refXN x = refX (show x)

refYN :: a -> Primitive
refYN x = refY (show x)

rxN :: a -> Primitive
rxN x = rx (show x)

ryN :: a -> Primitive
ryN x = ry (show x)

scaleN :: a -> Primitive
scaleN x = scale (show x)

startOffsetN :: a -> Primitive
startOffsetN x = startOffset (show x)

targetXN :: a -> Primitive
targetXN x = targetX (show x)

targetYN :: a -> Primitive
targetYN x = targetY (show x)

toN :: a -> Primitive
toN x = to (show x)

u1N :: a -> Primitive
u1N x = u1 (show x)

u2N :: a -> Primitive
u2N x = u2 (show x)

unitsPerEmN :: a -> Primitive
unitsPerEmN x = unitsPerEm (show x)

versionN :: a -> Primitive
versionN x = version (show x)

widthN :: a -> Primitive
widthN x = width (show x)

xN :: a -> Primitive
xN x' = x (show x')

x1N :: a -> Primitive
x1N x = x1 (show x)

x2N :: a -> Primitive
x2N x = x2 (show x)

yN :: a -> Primitive
yN x = y (show x)

y1N :: a -> Primitive
y1N x = y1 (show x)

y2N :: a -> Primitive
y2N x = y2 (show x)

zN :: a -> Primitive
zN x = z (show x)
