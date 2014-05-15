{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module Cinder.DSL
    (at
    ,el
    ,co
    ,markup
    ,pretty
    ,(!)
    ,(!:)
    ,(!<)
    ,(!<<)
    ,(!>)
    ,(!>>)
    ,(!+)
    ,(!<+)
    ,Primitive(..)
    ,Markup
    ,DString
    ,atN
    ,atP
    ,pr
    ,prN
    ) where

import           Fay.Text hiding (reverse)
import           Prelude  hiding (concat)

type DString = Text

data Primitive = Content DString
               | Element DString
               | Attribute DString DString
               | Property DString DString
               | Complete

type Markup = [Primitive]

markup :: Markup
markup = []

-- Recommend using this for Attributes and Content and (!>) for Elements
infixl 5 !
(!) :: Markup -> Primitive -> Markup
(!) = flip (:)

infixl 5 !:
(!:) :: Markup -> DString -> Markup
m !: v = m ! Content v

-- superfluous but maintains visual correspondence with (!<) when called
-- with Element.
infixl 5 !>
(!>) :: Markup -> Primitive -> Markup
(!>) = (!)

infixl 5 !>>
(!>>) :: Markup -> DString -> Markup
m !>> t = m ! Element t

infixl 5 !<
(!<) :: Markup -> Primitive -> Markup
m !< Complete = closeAll m
m !< t = (m ! Complete) ! t

-- allow inserting arbitrary element using just it's name
infixl 5 !<<
(!<<) :: Markup -> DString -> Markup
m !<< t = m !< Element t

infixl 5 !+
(!+) :: Markup -> Markup -> Markup
(!+) = flip (++)

infixl 5 !<+
(!<+) :: Markup -> Markup -> Markup
m !<+ t = (m ! Complete) !+ t

closeAll :: Markup -> Markup
closeAll m = if n >= 0 then replicate n Complete ++ m
                       else error $ unpack "Markup has more close elements than open"
    where n = foldr nestLevel 0 m

-- short constructors

at :: DString -> DString -> Primitive
at = Attribute

atN :: DString -> Double -> Primitive
atN t v = Attribute t (pack $ show v)

atP :: DString -> Double -> Primitive
atP t v = Attribute t (concat [ pack $ show v ,"%"])

pr :: DString -> DString -> Primitive
pr = Property

prN :: DString -> a -> Primitive
prN t v = Property t (pack $ show v)

co :: DString -> Primitive
co = Content

el :: DString -> Primitive
el = Element

-- simple pretty printer for debugging. convenient to use with putStrLn and
-- read in firebug or what gave you.
pretty :: Markup -> Text
pretty m = concat $ zipWith cat ins rm
    where rm = reverse m
          ins = scanl (flip nestLevel) 0 rm
          cat i x = concat [ pack $ replicate (i * 4) ' ' , go x , "\n"]
          go (Element   x)   = concat ["<" ,  x , ">"]
          go (Attribute x y) = concat [x , "=\"" , y , "\""]
          go (Property x y)  = concat [x , "=\"" , y , "\""]
          go (Content   x)   = concat ["\"" , x , "\""]
          go (Complete)      = "<--"

-- determine depth of markup tree.
nestLevel :: Primitive -> Int -> Int
nestLevel (Element _) x = x + 1
nestLevel (Complete)  x = x - 1
nestLevel _           x = x
