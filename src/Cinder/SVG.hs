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

ft :: String -> String -> Markup
ft f t = markup ! from f ! to t

ftN :: a -> a -> Markup
ftN f t = markup ! fromN f ! toN t

vs :: [String] -> Primitive
vs = values . isc

vsN :: [a] -> Primitive
vsN = vs . map show

ksC :: String -> Markup
ksC = (markup ! calcMode "spline" !) . keySplines

pc :: a -> String
pc = (++"%") . show

isc :: [String] -> String
isc = intercalate ";"

iscN :: [a] -> String
iscN = isc . map show

fxTimes :: Double -> Int -> Primitive
fxTimes b nb = keyTimes $ iscN $ 0 : (take (nb * 2)
                 [(1-b),((1-b) + b / fromIntegral (2*nb)) ..] ++ [1])

fxSplines :: Int -> Markup
fxSplines x = ksC $ isc $ "0 0 1 1"
                : replicate x "0 .75 .25 1; 1 .75 .25 0"

bounce :: Double -> Double -> Double -> Int -> Markup
bounce f t b nb = markup ! fxTimes b nb !+ fxSplines nb
                    ! vsN kv ! calcMode "spline"
    where kv = (intersperse t $ take (nb+1)
                    (map (t-) $ iterate (*b) (t-f))) ++ [t]

settle :: Double -> Double -> Double -> Int -> Markup
settle f t b nb = markup ! fxTimes b nb !+ fxSplines nb
                    ! vsN kv ! calcMode "spline"
    where kv = f : (take (nb*2) $ zipWith ($) (cycle [(t+),(t-)])
                                             (iterate (*b) (b*(t-f)))) ++ [t]

runningStart :: Double -> Double -> Double -> Markup
runningStart f t b = markup ! vsN [f,f - ((t-f)*b) ,t]
                            ! keyTimes (iscN [0,b / (1+2 * b),1])
                            !+ ksC "0 .75 .25 1; 1 .9 .1 0"

overShoot :: Double -> Double -> Double -> Markup
overShoot f t b = markup ! vsN [f, t + ((t-f)*b), t]
                            ! keyTimes (iscN [0,1 - (b / (1+2 * b)),1])
                            !+ ksC "1 .9 .1 0; 0 .75 .25 1"

--non-pure SVG specific stuff (animations and namespaces, mostly)
insert :: Markup -> Node -> Fay Node
insert = insertNS xmlns

node :: DString -> Fay Node
node = nodeNS xmlns

start :: Node -> Fay Node
start = ffi "%1['beginElement']() || %1"

stop :: Node -> Fay Node
stop = ffi "%1['endElement']() || %1"

-- values for attributes whose values might be animated

baseVal :: String -> Node -> Fay String
baseVal = ffi "%2['%1'].baseVal.value"

baseValN :: String -> Node -> Fay Double
baseValN = ffi "%2['%1'].baseVal.value"

animVal :: String -> Node -> Fay String
animVal = ffi "%2['%1'].animVal.value"

animValN :: String -> Node -> Fay Double
animValN = ffi "%2['%1'].animVal.value"

-- attributes whose names conflict with keywords
classA :: String -> Primitive
classA = Attribute "class"

typeA :: String -> Primitive
typeA = Attribute "type"

inA :: String -> Primitive
inA = Attribute "in"

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
beginN = Attribute "begin" . show

cxN :: a -> Primitive
cxN = Attribute "cx" . show

cyN :: a -> Primitive
cyN = Attribute "cy" . show

divisorN :: a -> Primitive
divisorN = Attribute "divisor" . show

durN :: a -> Primitive
durN = Attribute "dur" . show

dxN :: a -> Primitive
dxN = Attribute "dx" . show

dyN :: a -> Primitive
dyN = Attribute "dy" . show

elevationN :: a -> Primitive
elevationN = Attribute "elevation" . show

endN :: a -> Primitive
endN = Attribute "end" . show

exponentN :: a -> Primitive
exponentN = Attribute "exponent" . show

fromN :: a -> Primitive
fromN = Attribute "from" . show

fxN :: a -> Primitive
fxN = Attribute "fx" . show

fyN :: a -> Primitive
fyN = Attribute "fy" . show

heightN :: a -> Primitive
heightN = Attribute "height" . show

interceptN :: a -> Primitive
interceptN = Attribute "intercept" . show

kN :: a -> Primitive
kN = Attribute "k" . show

lengthAdjustN :: a -> Primitive
lengthAdjustN = Attribute "lengthAdjust" . show

maxN :: a -> Primitive
maxN = Attribute "max" . show

minN :: a -> Primitive
minN = Attribute "min" . show

opacityN :: a -> Primitive
opacityN = Attribute "opacity" . show

originN :: a -> Primitive
originN = Attribute "origin" . show

pathLengthN :: a -> Primitive
pathLengthN = Attribute "pathLength" . show

primitiveUnitsN :: a -> Primitive
primitiveUnitsN = Attribute "primitiveUnits" . show

rN :: a -> Primitive
rN = Attribute "r" . show

radiusN :: a -> Primitive
radiusN = Attribute "radius" . show

refXN :: a -> Primitive
refXN = Attribute "refX" . show

refYN :: a -> Primitive
refYN = Attribute "refY" . show

rxN :: a -> Primitive
rxN = Attribute "rx" . show

ryN :: a -> Primitive
ryN = Attribute "ry" . show

scaleN :: a -> Primitive
scaleN = Attribute "scale" . show

startOffsetN :: a -> Primitive
startOffsetN = Attribute "startOffset" . show

targetXN :: a -> Primitive
targetXN = Attribute "targetX" . show

targetYN :: a -> Primitive
targetYN = Attribute "targetY" . show

toN :: a -> Primitive
toN = Attribute "to" . show

u1N :: a -> Primitive
u1N = Attribute "u1" . show

u2N :: a -> Primitive
u2N = Attribute "u2" . show

unitsPerEmN :: a -> Primitive
unitsPerEmN = Attribute "unitsPerEm" . show

versionN :: a -> Primitive
versionN = Attribute "version" . show

widthN :: a -> Primitive
widthN = Attribute "width" . show

xN :: a -> Primitive
xN = Attribute "x" . show

x1N :: a -> Primitive
x1N = Attribute "x1" . show

x2N :: a -> Primitive
x2N = Attribute "x2" . show

yN :: a -> Primitive
yN = Attribute "y" . show

y1N :: a -> Primitive
y1N = Attribute "y1" . show

y2N :: a -> Primitive
y2N = Attribute "y2" . show

zN :: a -> Primitive
zN = Attribute "z" . show
