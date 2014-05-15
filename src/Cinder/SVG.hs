{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module Cinder.SVG
    (module Cinder.SVG, module Cinder.DOM, module Cinder.DSL)
    where

import           Cinder.DOM            hiding (toLower)
import           Cinder.DSL
import           Cinder.SVG.Attributes as A hiding (path, r, rx, ry, x, y, z)
import           Cinder.SVG.Elements   as E hiding (a)
import           Control.Fay
import           Fay.Text
import           FFI
import           Prelude               as P hiding (concat, concatMap,
                                             intercalate, max, min)

xmlns :: Text
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

showD :: [Seg] -> Text
showD = concat . P.map hShow

showDRelative :: [Seg] -> Text
showDRelative = toLower . showD

dS :: [Seg] -> Primitive
dS = A.d . showD

dSR :: [Seg] -> Primitive
dSR = A.d . showDRelative

pathD :: [Seg] -> Markup
pathD = (markup ! path !) . dS

pathDR :: [Seg] -> Markup
pathDR = (markup ! path !) . dSR

-- transform stuff

data Transform = Translate Double Double
               | Rotate Double Double Double
               | Scale Double
               | Scale2d Double Double

transformType :: Transform -> Text
transformType Translate {} = "translate"
transformType Rotate {}    = "rotate"
transformType Scale {}     = "scale"
transformType Scale2d {}   = "scale"

transformVals :: Transform -> Text
transformVals (Translate x y) = concat [ toText x , "," , toText y ]
transformVals (Rotate x y z)  = concat [ toText x , "," , toText y , "," , toText z ]
transformVals (Scale x)       = toText x
transformVals (Scale2d x y)   = concat [ toText x , "," , toText y ]

transformT :: [Transform] -> Primitive
transformT = transform . intercalate "," . P.map go
    where go t = concat [ transformType t , "(" , transformVals t ,  ")" ]

-- animation stuff

-- A = attributeName, T = to, B = begin
setATB :: Text -> Text -> a -> Markup
setATB a t b = markup ! set ! attributeName a ! to t ! beginN b

-- D = dur, R = repeatCount
animationADR :: Text -> a -> Double -> Markup
animationADR a duration r = markup ! attributeName a ! durN duration ! repeatCount (go r)
                     where go x | x > 0 = toText x
                           go _         = "indefinite"

aADR :: Text -> a -> Double -> Markup
aADR = (((markup ! animate !+) .) .) . animationADR

atDRT :: a -> Double -> [Transform] -> Markup
atDRT duration r ts = markup ! animateTransform !+ animationADR "transform" duration r
               ! at "type" (transformType $ P.head ts) ! vs (P.map transformVals ts)

beIndef :: Markup
beIndef = markup ! begin "indefinite" ! end "indefinite"

-- f = from, t = to
ft :: Text -> Text -> Markup
ft f t = markup ! from f ! to t

ftN :: a -> a -> Markup
ftN f t = markup ! fromN f ! toN t

-- turn list into SVG style values list
vs :: [Text] -> Primitive
vs = values . isc

vsN :: [a] -> Primitive
vsN = vs . P.map toText

ksC :: Text -> Markup
ksC = (markup ! calcMode "spline" !) . keySplines

pc :: a -> Text
pc x  = concat [ toText x, "%" ]

-- slip in semicolon
isc :: [Text] -> Text
isc = intercalate ";"

iscN :: [a] -> Text
iscN = isc . P.map toText

-- draw equilateral polygons n = number of sides, r = distance from center
-- to vertex
-- ngon :: Double -> Double -> Double -> Double -> [Seg]
-- ngon n x y r = go M 0 : map (go L) [1 .. n - 1] ++ [Z]
--     where go f i = f (x + r * cos (c * i)) (y + r * sin (c * i))
--           c = 2.0 * pi / n

-- Helper func for bounce and settle, which see
bounceFX :: (Double -> Int -> Double -> Double -> [Double]) ->
    Double -> Int -> Double -> Double -> Markup
bounceFX kv bo num f t = markup ! ts bo num !+ ss num ! vsN (kv bo num f t ++ [t])
    where ts b nb = keyTimes $ iscN $ 0 : (take (nb * 2)
                 [(1-b),((1-b) + b / fromIntegral (2*nb)) ..] ++ [1])
          ss x = ksC $ isc $ "0 0 1 1" : replicate x "0 .75 .25 1; 1 .75 .25 0"

-- animate attribute such that it returns frac percent of way to original
-- and repeat cnt times. f = original t = final (from, to)
bounce :: Double -> Int -> Double -> Double -> Markup
bounce frac cnt begPos endPos = bounceFX kv frac cnt begPos endPos
    where kv b nb f t = P.intersperse t $ P.take (nb+1) (P.map (t-)
                            $ iterate (*b) (t-f))

-- overshoot and return repeatedly, like bounce
settle :: Double -> Int -> Double -> Double -> Markup
settle = bounceFX kv
    where kv b nb f t = f : take (nb*2) (zipWith ($) (cycle [(t+),(t-)])
                                             (iterate (*b) (b*(t-f))))

-- start by animating in opposite direction b percent
runningStart :: Double -> Double -> Double -> Markup
runningStart b f t = markup ! vsN [f,f - ((t-f)*b) ,t]
                            ! keyTimes (iscN [0,b / (1+2 * b),1])
                            !+ ksC "0 .75 .25 1; 1 .9 .1 0"

-- go past target b percent and return
overShoot :: Double -> Double -> Double -> Markup
overShoot b f t = markup ! vsN [f, t + ((t-f)*b), t]
                            ! keyTimes (iscN [0,1 - (b / (1+2 * b)),1])
                            !+ ksC "1 .9 .1 0; 0 .75 .25 1"

speedUp :: Markup
speedUp = ksC "1 .8 .3 .1"

slowDown :: Markup
slowDown = ksC "0 .2 .8 1"

-- animate tracing path with current stroke for r repetitions of d duration
-- l = path length. Pure func called from tracePath
trace :: Double -> Double -> Double -> Markup
trace duration r l = markup ! strokeDashoffset (toText l)
              ! strokeDasharray (concat [ toText l, "," ,toText l])
              !+ aADR "stroke-dashoffset" duration r ! fill "freeze" !+ ftN l 0

--non-pure SVG specific stuff (animations and namespaces, mostly)

insert :: Markup -> Node -> Fay Node
insert = insertNS xmlns

node :: DString -> Fay Node
node = nodeNS xmlns

-- start an animation
start :: Node -> Fay Node
start = ffi "%1['beginElement']() || %1"

-- stop an animation
stop :: Node -> Fay Node
stop = ffi "%1['endElement']() || %1"

-- zip Markup to Node using insert
zipInsert :: [Markup] -> [Node] -> Fay [Node]
zipInsert = zipWithM insert

-- stagger an a attribute starting with s and incrementing i across list of
-- Nodes
stagger :: Text -> Double -> Double -> [Node] -> Fay [Node]
stagger a s i = zipInsert $ P.map go [s, s + i ..]
        where go x = markup ! Attribute a (toText x)

-- get length of all segments of path node
totalLength :: Node -> Fay Double
totalLength = ffi "%1['getTotalLength']()"

-- animate tracing path with current stroke for r repetitions of d duration
-- impure because path length must be determined
tracePath :: Double -> Double -> Node -> Fay Node
tracePath duration r n = do l <- totalLength n
                            _ <- insert (trace duration r l) n
                            lastChild n

-- values for attributes whose values might be animated

baseVal :: Text -> Node -> Fay Text
baseVal = ffi "%2['%1']['baseVal']"

baseValN :: Text -> Node -> Fay Double
baseValN = ffi "%2['%1']['baseVal']"

animVal :: Text -> Node -> Fay Text
animVal = ffi "%2['%1']['animVal']"

animValN :: Text -> Node -> Fay Double
animValN = ffi "%2['%1']['animVal']"

-- global pause and unpause animations

pauseAll :: Fay ()
pauseAll = ffi "document['documentElement']['pauseAnimations']()"

unpauseAll :: Fay ()
unpauseAll = ffi "document['documentElement']['unpauseAnimations']()"

-- attributes whose names conflict with keywords
classA :: Text -> Primitive
classA = Attribute "class"

typeA :: Text -> Primitive
typeA = Attribute "type"

inA :: Text -> Primitive
inA = Attribute "in"

-- HACK!!! to mimic Haskell's show instead of getting JSON

inst :: Automatic a -> Text
inst = ffi "%1['instance'] + ' '"

slot :: Int -> Automatic a -> Text
slot = ffi "(%2['slot'+%1] != null && (%2['slot'+%1] + ' ')) || ''"

hShow :: Automatic a -> Text
hShow x = concat $ inst x : P.map (`slot` x) [1 .. 8]

-- (possibly) numeric attributes

beginN :: a -> Primitive
beginN = Attribute "begin" . toText

cxN :: a -> Primitive
cxN = Attribute "cx" . toText

cyN :: a -> Primitive
cyN = Attribute "cy" . toText

divisorN :: a -> Primitive
divisorN = Attribute "divisor" . toText

durN :: a -> Primitive
durN = Attribute "dur" . toText

dxN :: a -> Primitive
dxN = Attribute "dx" . toText

dyN :: a -> Primitive
dyN = Attribute "dy" . toText

elevationN :: a -> Primitive
elevationN = Attribute "elevation" . toText

endN :: a -> Primitive
endN = Attribute "end" . toText

exponentN :: a -> Primitive
exponentN = Attribute "exponent" . toText

fromN :: a -> Primitive
fromN = Attribute "from" . toText

fxN :: a -> Primitive
fxN = Attribute "fx" . toText

fyN :: a -> Primitive
fyN = Attribute "fy" . toText

heightN :: a -> Primitive
heightN = Attribute "height" . toText

interceptN :: a -> Primitive
interceptN = Attribute "intercept" . toText

kN :: a -> Primitive
kN = Attribute "k" . toText

lengthAdjustN :: a -> Primitive
lengthAdjustN = Attribute "lengthAdjust" . toText

maxN :: a -> Primitive
maxN = Attribute "max" . toText

minN :: a -> Primitive
minN = Attribute "min" . toText

opacityN :: a -> Primitive
opacityN = Attribute "opacity" . toText

originN :: a -> Primitive
originN = Attribute "origin" . toText

pathLengthN :: a -> Primitive
pathLengthN = Attribute "pathLength" . toText

primitiveUnitsN :: a -> Primitive
primitiveUnitsN = Attribute "primitiveUnits" . toText

rN :: a -> Primitive
rN = Attribute "r" . toText

radiusN :: a -> Primitive
radiusN = Attribute "radius" . toText

refXN :: a -> Primitive
refXN = Attribute "refX" . toText

refYN :: a -> Primitive
refYN = Attribute "refY" . toText

rxN :: a -> Primitive
rxN = Attribute "rx" . toText

ryN :: a -> Primitive
ryN = Attribute "ry" . toText

scaleN :: a -> Primitive
scaleN = Attribute "scale" . toText

startOffsetN :: a -> Primitive
startOffsetN = Attribute "startOffset" . toText

targetXN :: a -> Primitive
targetXN = Attribute "targetX" . toText

targetYN :: a -> Primitive
targetYN = Attribute "targetY" . toText

toN :: a -> Primitive
toN = Attribute "to" . toText

u1N :: a -> Primitive
u1N = Attribute "u1" . toText

u2N :: a -> Primitive
u2N = Attribute "u2" . toText

unitsPerEmN :: a -> Primitive
unitsPerEmN = Attribute "unitsPerEm" . toText

versionN :: a -> Primitive
versionN = Attribute "version" . toText

widthN :: a -> Primitive
widthN = Attribute "width" . toText

xN :: a -> Primitive
xN = Attribute "x" . toText

x1N :: a -> Primitive
x1N = Attribute "x1" . toText

x2N :: a -> Primitive
x2N = Attribute "x2" . toText

yN :: a -> Primitive
yN = Attribute "y" . toText

y1N :: a -> Primitive
y1N = Attribute "y1" . toText

y2N :: a -> Primitive
y2N = Attribute "y2" . toText

zN :: a -> Primitive
zN = Attribute "z" . toText
