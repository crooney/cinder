module Cinder.DOM (module Cinder.DOM, module Cinder.DSL) where

import Prelude
import FFI
import Control.Fay (foldM)
import Cinder.DSL

data Node

root :: Fay Node
root = ffi "document['documentElement']"

nodeNS :: String -> String -> Fay Node
nodeNS = ffi "document['createElementNS'](%1,%2)"

content :: String -> Fay Node
content = ffi "document['createTextNode'](%1)"

clone :: Bool -> Node -> Fay Node
clone = ffi "%2['cloneNode'](%1)"

setChild :: Node -> Node -> Fay Node
setChild = ffi "(%2['appendChild'](%1) && null) || %2"

setParent :: Node -> Node -> Fay Node
setParent = ffi "(%1['appendChild'](%2) && null) || %2"

setAttribute :: String -> String -> Node -> Fay Node
setAttribute = ffi "(%3['setAttributeNS'](null,%1,%2) && null) || %3"

setProperty :: String -> String -> Node -> Fay Node
setProperty = ffi "((%3[%1] = %2) && null) || %3"

deleteSelf :: Node -> Fay ()
deleteSelf = ffi "%1['parentNode']['removeChild'](%1)"

deleteChild :: Node -> Node -> Fay Node
deleteChild = ffi "(%1['removeChild'](%2) && null) || %1"

replace :: Node -> Node -> Fay Node
replace = ffi "%1['parentNode']['replaceChild'](%2,%1) || %2"

byId :: String -> Fay Node
byId = ffi "document['getElementById'](%1)"

byTag :: String -> Node -> Fay [Node]
byTag = ffi "%2['getElementsByTagName'](%1)"

byClass :: String -> Node -> Fay [Node]
byClass = ffi "%2['getElementsByClassName'](%1)"

parent :: Node -> Fay Node
parent = ffi "%1['parentNode']"

firstChild :: Node -> Fay Node
firstChild = ffi "%1['firstChild']"

lastChild :: Node -> Fay Node
lastChild = ffi "%1['lastChild']"

attributeN :: String -> Node -> Fay Double
attributeN = ffi "%2['getAttributeNS'](null,%1)"

attribute :: String -> Node -> Fay String
attribute = ffi "%2['getAttributeNS'](null,%1)"

property :: String -> Node -> Fay String
property = ffi "%2['%1']"

propertyN :: String -> Node -> Fay Double
propertyN = ffi "%2['%1']"

toLower :: String -> String
toLower = ffi "String(%1)['toLowerCase']()"

rgb :: (Int, Int, Int) -> String
rgb (r, g, b) = "rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

rgba :: (Int, Int, Int, Int) -> String
rgba (r, g, b, a) = "rgba(" ++ show r ++ "," ++ show g ++ "," ++ show b
                    ++ "," ++ show a ++ ")"

insertNS :: String -> Markup -> Node -> Fay Node
insertNS s m n = foldM go n (reverse m)
    where go n (Attribute k v) = setAttribute k v n
          go n (Element t)     = nodeNS s t >>= setParent n
          go n (Complete)      = parent n
          go n (Property k v)  = setProperty k v n
          go n (Content v)     = content v >>= setParent n >> return n

