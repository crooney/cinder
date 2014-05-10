{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module Cinder.DOM (module Cinder.DOM, module Cinder.DSL) where

import           Cinder.DSL
import           Control.Fay (foldM)
import           Fay.Text    hiding (reverse)
import           FFI
import           Prelude     hiding (concat)

data Node

root :: Fay Node
root = ffi "document['documentElement']"

nodeNS :: Text -> Text -> Fay Node
nodeNS = ffi "document['createElementNS'](%1,%2)"

content :: Text -> Fay Node
content = ffi "document['createTextNode'](%1)"

clone :: Bool -> Node -> Fay Node
clone = ffi "%2['cloneNode'](%1)"

setChild :: Node -> Node -> Fay Node
setChild = ffi "(%2['appendChild'](%1) && null) || %2"

setParent :: Node -> Node -> Fay Node
setParent = ffi "(%1['appendChild'](%2) && null) || %2"

setAttribute :: Text -> Text -> Node -> Fay Node
setAttribute = ffi "(%3['setAttributeNS'](null,%1,%2) && null) || %3"

setProperty :: Text -> Text -> Node -> Fay Node
setProperty = ffi "((%3[%1] = %2) && null) || %3"

deleteSelf :: Node -> Fay ()
deleteSelf = ffi "%1['parentNode']['removeChild'](%1)"

deleteChild :: Node -> Node -> Fay Node
deleteChild = ffi "(%1['removeChild'](%2) && null) || %1"

replace :: Node -> Node -> Fay Node
replace = ffi "%1['parentNode']['replaceChild'](%2,%1) || %2"

byId :: Text -> Fay Node
byId = ffi "document['getElementById'](%1)"

byTag :: Text -> Node -> Fay [Node]
byTag = ffi "%2['getElementsByTagName'](%1)"

byClass :: Text -> Node -> Fay [Node]
byClass = ffi "%2['getElementsByClassName'](%1)"

parent :: Node -> Fay Node
parent = ffi "%1['parentNode']"

firstChild :: Node -> Fay Node
firstChild = ffi "%1['firstChild']"

lastChild :: Node -> Fay Node
lastChild = ffi "%1['lastChild']"

attributeN :: Text -> Node -> Fay Double
attributeN = ffi "%2['getAttributeNS'](null,%1)"

attribute :: Text -> Node -> Fay Text
attribute = ffi "%2['getAttributeNS'](null,%1)"

property :: Text -> Node -> Fay Text
property = ffi "%2['%1']"

propertyN :: Text -> Node -> Fay Double
propertyN = ffi "%2['%1']"

toLower :: Text -> Text
toLower = ffi "String(%1)['toLowerCase']()"

toText :: Automatic a -> Text
toText  = pack . show

rgb :: (Int, Int, Int) -> Text
rgb (r, g, b) = concat ["rgb(" , toText r , "," , toText g , "," , toText b , ")"]

rgba :: (Int, Int, Int, Int) -> Text
rgba (r, g, b, a) = concat ["rgba(" , toText r , "," , toText g , "," , toText b
                    , "," , toText a , ")"]

insertNS :: Text -> Markup -> Node -> Fay Node
insertNS s m node = foldM go node (reverse m)
    where go n (Attribute k v) = setAttribute k v n
          go n (Element t)     = nodeNS s t >>= setParent n
          go n (Complete)      = parent n
          go n (Property k v)  = setProperty k v n
          go n (Content v)     = content v >>= setParent n >> return n

