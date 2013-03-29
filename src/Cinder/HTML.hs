module Cinder.HTML
    (module Cinder.HTML, module Cinder.DOM, module Cinder.DSL)
    where

import Prelude hiding (min, max, div)
import FFI
import Cinder.DSL
import Cinder.DOM
import Cinder.HTML.Attributes
import Cinder.HTML.Elements

xmlns :: String
xmlns = "http://www.w3.org/1999/xhtml"

imgS :: String -> Markup
imgS = (markup !) . src

imgSA :: String -> String -> Markup
imgSA s a = imgS s ! alt a

eC :: Primitive -> String -> Markup
eC e c = markup ! e !: c

eCC :: Primitive -> String -> String -> Markup
eCC e c cl = eC e c ! classA cl

dC :: String -> Markup
dC = eC div

dCC :: String -> String -> Markup
dCC = eCC div

pC :: String -> Markup
pC = eC p

pCC :: String -> String -> Markup
pCC = eCC p

iC :: String -> Markup
iC = eC i

bC :: String -> Markup
bC = eC b

strongC :: String -> Markup
strongC = eC strong

--non-pure HTML specific stuff
insert :: Markup -> Node -> Fay Node
insert = insertNS xmlns

node :: DString -> Fay Node
node = nodeNS xmlns

-- attributes whose names conflict with keywords
classA :: String -> Primitive
classA = Attribute "class"

typeA :: String -> Primitive
typeA = Attribute "type"

dataA :: String -> Primitive
dataA = Attribute "data"

defaultA :: String -> Primitive
defaultA = Attribute "default"

-- Boolean Attributes
asyncB :: Primitive
asyncB = Attribute "async" ""

autofocusB :: Primitive
autofocusB = Attribute "autofocus" ""

autoplayB :: Primitive
autoplayB = Attribute "autoplay" ""

checkedB :: Primitive
checkedB = Attribute "checked" ""

controlsB :: Primitive
controlsB = Attribute "controls" ""

defaultB :: Primitive
defaultB = Attribute "default" ""

deferB :: Primitive
deferB = Attribute "defer" ""

disabledB :: Primitive
disabledB = Attribute "disabled" ""

formnovalidateB :: Primitive
formnovalidateB = Attribute "formnovalidate" ""

hiddenB :: Primitive
hiddenB = Attribute "hidden" ""

ismapB :: Primitive
ismapB = Attribute "ismap" ""

loopB :: Primitive
loopB = Attribute "loop" ""

multipleB :: Primitive
multipleB = Attribute "multiple" ""

mutedB :: Primitive
mutedB = Attribute "muted" ""

novalidateB :: Primitive
novalidateB = Attribute "novalidate" ""

openB :: Primitive
openB = Attribute "open" ""

readonlyB :: Primitive
readonlyB = Attribute "readonly" ""

requiredB :: Primitive
requiredB = Attribute "required" ""

reversedB :: Primitive
reversedB = Attribute "reversed" ""

scopedB :: Primitive
scopedB = Attribute "scoped" ""

seamlessB :: Primitive
seamlessB = Attribute "seamless" ""

selectedB :: Primitive
selectedB = Attribute "selected" ""

typemustmatchB :: Primitive
typemustmatchB = Attribute "typemustmatch" ""

-- (possibly) numeric attributes
colsN :: a -> Primitive
colsN = Attribute "cols" . show

colspanN :: a -> Primitive
colspanN = Attribute "colspan" . show

coordsN :: a -> Primitive
coordsN = Attribute "coords" . show

datetimeN :: a -> Primitive
datetimeN = Attribute "datetime" . show

heightN :: a -> Primitive
heightN = Attribute "height" . show

highN :: a -> Primitive
highN = Attribute "high" . show

lowN :: a -> Primitive
lowN = Attribute "low" . show

maxN :: a -> Primitive
maxN = Attribute "max" . show

maxlengthN :: a -> Primitive
maxlengthN = Attribute "maxlength" . show

minN :: a -> Primitive
minN = Attribute "min" . show

optimumN :: a -> Primitive
optimumN = Attribute "optimum" . show

rowsN :: a -> Primitive
rowsN = Attribute "rows" . show

rowspanN :: a -> Primitive
rowspanN = Attribute "rowspan" . show

sizeN :: a -> Primitive
sizeN = Attribute "size" . show

spanN :: a -> Primitive
spanN = Attribute "span" . show

startN :: a -> Primitive
startN = Attribute "start" . show

stepN :: a -> Primitive
stepN = Attribute "step" . show

tabindexN :: a -> Primitive
tabindexN = Attribute "tabindex" . show

valueN :: a -> Primitive
valueN = Attribute "value" . show

widthN :: a -> Primitive
widthN = Attribute "width" . show

