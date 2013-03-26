module Cinder.SVG
    (module Cinder.SVG, module Cinder.DOM, module Cinder.DSL)
    where

import Prelude hiding (min, max)
import FFI
import Cinder.DSL
import Cinder.DOM
import Cinder.SVG.Attributes hiding (path)
import Cinder.SVG.Elements

xmlns :: String
xmlns = "http://www.w3.org/2000/svg"

-- convenience methods for simple shapes

cR :: a -> Markup
cR = (markup ! circle ! ) . rN

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

--path stuff

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

pathD :: [Seg] -> Markup
pathD = (markup ! path !) . dS

pathDR :: [Seg] -> Markup
pathDR = (markup ! path !) . dSR

-- transform stuff

data Transform = Translate Double Double
               | Rotate Double Double Double
               | Scale Double
               | Scale2d Double Double

transformType :: Transform -> String
transformType Translate {} = "translate"
transformType Rotate {}    = "rotate"
transformType Scale {}     = "scale"
transformType Scale2d {}   = "scale"

transformVals :: Transform -> String
transformVals (Translate x y) = show x ++ "," ++ show y
transformVals (Rotate x y z)  = show x ++ "," ++ show y ++ "," ++ show z
transformVals (Scale x)       = show x
transformVals (Scale2d x y)   = show x ++ "," ++ show y

transformT :: [Transform] -> Primitive
transformT = transform . intercalate "," . map go
    where go t = transformType t ++ "(" ++ transformVals t ++ ")"

-- animation stuff

setATB :: String -> String -> a -> Markup
setATB a t b = markup ! set ! attributeName a ! to t ! beginN b

animationADR :: String -> a -> Double -> Markup
animationADR a d r = markup ! attributeName a ! durN d ! repeatCount (go r)
                     where go x | x > 0 = show x
                           go _         = "indefinite"

aADR :: String -> a -> Double -> Markup
aADR = (((markup ! animate !+) .) .) . animationADR

atDRT :: a -> Double -> [Transform] -> Markup
atDRT d r ts = markup ! animateTransform !+ animationADR "transform" d r
               ! at "type" (transformType $ head ts) ! vs (map transformVals ts)

beIndef :: Markup
beIndef = markup ! begin "indefinite" ! end "indefinite"

ft :: a -> a -> Markup
ft f t = markup ! fromN f ! toN t

vs :: [String] -> Primitive
vs = values . intercalate ";"

vsN :: [a] -> Primitive
vsN = vs . map show

pc :: a -> String
pc = (++"%") . show

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
beginN = begin . show

cxN :: a -> Primitive
cxN = cx . show

cyN :: a -> Primitive
cyN = cy . show

divisorN :: a -> Primitive
divisorN = divisor . show

durN :: a -> Primitive
durN = dur . show

dxN :: a -> Primitive
dxN = dx . show

dyN :: a -> Primitive
dyN = dy . show

elevationN :: a -> Primitive
elevationN = elevation . show

endN :: a -> Primitive
endN = end . show

exponentN :: a -> Primitive
exponentN = exponent . show

fromN :: a -> Primitive
fromN = from . show

fxN :: a -> Primitive
fxN = fx . show

fyN :: a -> Primitive
fyN = fy . show

heightN :: a -> Primitive
heightN = height . show

interceptN :: a -> Primitive
interceptN = intercept . show

kN :: a -> Primitive
kN = k . show

lengthAdjustN :: a -> Primitive
lengthAdjustN = lengthAdjust . show

maxN :: a -> Primitive
maxN = max . show

minN :: a -> Primitive
minN = min . show

originN :: a -> Primitive
originN = origin . show

pathLengthN :: a -> Primitive
pathLengthN = pathLength . show

primitiveUnitsN :: a -> Primitive
primitiveUnitsN = primitiveUnits . show

rN :: a -> Primitive
rN = r . show

radiusN :: a -> Primitive
radiusN = radius . show

refXN :: a -> Primitive
refXN = refX . show

refYN :: a -> Primitive
refYN = refY . show

rxN :: a -> Primitive
rxN = rx . show

ryN :: a -> Primitive
ryN = ry . show

scaleN :: a -> Primitive
scaleN = scale . show

startOffsetN :: a -> Primitive
startOffsetN = startOffset . show

targetXN :: a -> Primitive
targetXN = targetX . show

targetYN :: a -> Primitive
targetYN = targetY . show

toN :: a -> Primitive
toN = to . show

u1N :: a -> Primitive
u1N = u1 . show

u2N :: a -> Primitive
u2N = u2 . show

unitsPerEmN :: a -> Primitive
unitsPerEmN = unitsPerEm . show

versionN :: a -> Primitive
versionN = version . show

widthN :: a -> Primitive
widthN = width . show

xN :: a -> Primitive
xN = x . show

x1N :: a -> Primitive
x1N = x1 . show

x2N :: a -> Primitive
x2N = x2 . show

yN :: a -> Primitive
yN = y . show

y1N :: a -> Primitive
y1N = y1 . show

y2N :: a -> Primitive
y2N = y2 . show

zN :: a -> Primitive
zN = z . show
