{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}
module Cinder.HTML
    (module Cinder.HTML, module Cinder.DOM, module Cinder.DSL)
    where

import           Cinder.DOM
import           Cinder.DSL
import           Cinder.HTML.Attributes
import           Cinder.HTML.Elements
import           Fay.Text
import           FFI
import           Prelude                hiding (div, max, min)

xmlns :: Text
xmlns = "http://www.w3.org/1999/xhtml"

-- Various convenience funcs

imgS :: Text -> Markup
imgS = (markup ! img !) . src

imgSA :: Text -> Text -> Markup
imgSA s a = imgS s ! alt a

-- e = Element, C = class, X = Content, I = id

eX :: Primitive -> Text -> Markup
eX e c = markup ! e !: c

eCX :: Primitive -> Text -> Text -> Markup
eCX e cl c = eX e c ! classA cl

dX :: Text -> Markup
dX = eX div

dCX :: Text -> Text -> Markup
dCX = eCX div

pX :: Text -> Markup
pX = eX p

pCX :: Text -> Text -> Markup
pCX = eCX p

iX :: Text -> Markup
iX = eX i

bX :: Text -> Markup
bX = eX b

strongX :: Text -> Markup
strongX = eX strong

-- non-pure HTML specific stuff

insert :: Markup -> Node -> Fay Node
insert = insertNS xmlns

node :: DString -> Fay Node
node = nodeNS xmlns

-- attributes whose names conflict with keywords

classA :: Text -> Primitive
classA = Attribute "class"

typeA :: Text -> Primitive
typeA = Attribute "type"

dataA :: Text -> Primitive
dataA = Attribute "data"

defaultA :: Text -> Primitive
defaultA = Attribute "default"

-- Convenience Properties

inner :: Text -> Primitive
inner = Property "innerHTML"

-- Boolean Attributes
-- in the w3c sense of boolean -- not true or false, rather present or not

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

