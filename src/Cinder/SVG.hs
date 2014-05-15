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

cR :: Double -> Markup
cR = (markup ! circle ! ) . rN

cXYR :: Double -> Double -> Double -> Markup
cXYR x y r = cRXY r x y

cRXY :: Double -> Double -> Double -> Markup
cRXY r x y = cR r ! cxN x ! cyN y

rHW :: Double -> Double -> Markup
rHW h w = markup ! rect ! heightN h ! widthN w

rHWXY :: Double -> Double -> Double -> Double -> Markup
rHWXY h w x y = rHW h w ! xN x ! yN y

rXYHW :: Double -> Double -> Double -> Double -> Markup
rXYHW x y h w = rHWXY h w x y

eRR :: Double -> Double -> Markup
eRR rx ry = markup ! ellipse ! rxN rx ! ryN ry

eRRXY :: Double -> Double -> Double -> Double -> Markup
eRRXY rx ry x y = eRR rx ry ! cxN x ! cyN y

lXYXY :: Double -> Double -> Double -> Double -> Markup
lXYXY x y x' y' = markup ! line ! x1N x ! y1N y ! x2N x' ! y2N y'

lXXYY :: Double -> Double -> Double -> Double -> Markup
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
showD = concat . P.map (intercalate " " . go)
          where go (M x y)           = [ " M" , toText x , toText y ]
                go (L x y)           = [ " L" , toText x , toText y ]
                go (H x)             = [ " H" , toText x ]
                go (V x)             = [ " V" , toText x ]
                go (A x y z a b c w) = [ " A" , toText x , toText y , toText z ,
                  toText a , toText b , toText c , toText w ]
                go (C x y z a b c)   = [ " C" , toText x , toText y , toText z ,
                  toText a , toText b , toText c ]
                go (S x y z a)       = [ " S" , toText x , toText y , toText z ,
                  toText a ]
                go (Q x y z a)       = [ " Q" , toText x , toText y , toText z ,
                  toText a ]
                go (T x y)           = [ " T" , toText x , toText y ]
                go _                 = [ " Z" ]

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
setATB :: Text -> Text -> Double -> Markup
setATB a t b = markup ! set ! attributeName a ! to t ! beginN b

-- D = dur, R = repeatCount
animationADR :: Text -> Double -> Double -> Markup
animationADR a duration r = markup ! attributeName a ! durN duration ! repeatCount (go r)
                     where go x | x > 0 = toText x
                           go _         = "indefinite"

aADR :: Text -> Double -> Double -> Markup
aADR = (((markup ! animate !+) .) .) . animationADR

atDRT :: Double -> Double -> [Transform] -> Markup
atDRT duration r ts = markup ! animateTransform !+ animationADR "transform" duration r
               ! at "type" (transformType $ P.head ts) ! vs (P.map transformVals ts)

beIndef :: Markup
beIndef = markup ! begin "indefinite" ! end "indefinite"

-- f = from, t = to
ft :: Text -> Text -> Markup
ft f t = markup ! from f ! to t

ftN :: Double -> Double -> Markup
ftN f t = markup ! fromN f ! toN t

-- turn list into SVG style values list
vs :: [Text] -> Primitive
vs = values . isc

vsN :: [Double] -> Primitive
vsN = vs . P.map toText

ksC :: Text -> Markup
ksC = (markup ! calcMode "spline" !) . keySplines

pc :: Double -> Text
pc x  = concat [ toText x, "%" ]

-- slip in semicolon
isc :: [Text] -> Text
isc = intercalate ";"

iscN :: [Double] -> Text
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

-- deprecated in favor of `show` instances
hShow :: Automatic a -> Text
hShow = fromString . show

-- (possibly) numeric attributes

beginN :: Double -> Primitive
beginN = atN "begin"

cxN :: Double -> Primitive
cxN = atN "cx"

cyN :: Double -> Primitive
cyN = atN "cy"

divisorN :: Double -> Primitive
divisorN = atN "divisor"

durN :: Double -> Primitive
durN = atN "dur"

dxN :: Double -> Primitive
dxN = atN "dx"

dyN :: Double -> Primitive
dyN = atN "dy"

elevationN :: Double -> Primitive
elevationN = atN "elevation"

endN :: Double -> Primitive
endN = atN "end"

exponentN :: Double -> Primitive
exponentN = atN "exponent"

fromN :: Double -> Primitive
fromN = atN "from"

fxN :: Double -> Primitive
fxN = atN "fx"

fyN :: Double -> Primitive
fyN = atN "fy"

heightN :: Double -> Primitive
heightN = atN "height"

interceptN :: Double -> Primitive
interceptN = atN "intercept"

kN :: Double -> Primitive
kN = atN "k"

lengthAdjustN :: Double -> Primitive
lengthAdjustN = atN "lengthAdjust"

maxN :: Double -> Primitive
maxN = atN "max"

minN :: Double -> Primitive
minN = atN "min"

opacityN :: Double -> Primitive
opacityN = atN "opacity"

originN :: Double -> Primitive
originN = atN "origin"

pathLengthN :: Double -> Primitive
pathLengthN = atN "pathLength"

primitiveUnitsN :: Double -> Primitive
primitiveUnitsN = atN "primitiveUnits"

rN :: Double -> Primitive
rN = atN "r"

radiusN :: Double -> Primitive
radiusN = atN "radius"

refXN :: Double -> Primitive
refXN = atN "refX"

refYN :: Double -> Primitive
refYN = atN "refY"

rxN :: Double -> Primitive
rxN = atN "rx"

ryN :: Double -> Primitive
ryN = atN "ry"

scaleN :: Double -> Primitive
scaleN = atN "scale"

startOffsetN :: Double -> Primitive
startOffsetN = atN "startOffset"

targetXN :: Double -> Primitive
targetXN = atN "targetX"

targetYN :: Double -> Primitive
targetYN = atN "targetY"

toN :: Double -> Primitive
toN = atN "to"

u1N :: Double -> Primitive
u1N = atN "u1"

u2N :: Double -> Primitive
u2N = atN "u2"

unitsPerEmN :: Double -> Primitive
unitsPerEmN = atN "unitsPerEm"

versionN :: Double -> Primitive
versionN = atN "version"

widthN :: Double -> Primitive
widthN = atN "width"

xN :: Double -> Primitive
xN = atN "x"

x1N :: Double -> Primitive
x1N = atN "x1"

x2N :: Double -> Primitive
x2N = atN "x2"

yN :: Double -> Primitive
yN = atN "y"

y1N :: Double -> Primitive
y1N = atN "y1"

y2N :: Double -> Primitive
y2N = atN "y2"

zN :: Double -> Primitive
zN = atN "z"
