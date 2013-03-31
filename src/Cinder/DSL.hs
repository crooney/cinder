{-# LANGUAGE NoImplicitPrelude #-}

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
    ) where

import Prelude

type DString = String

data Primitive = Content DString
               | Element DString
               | Attribute DString DString
               | Complete
    deriving Show

type Markup = [Primitive]

markup :: Markup
markup = []

infixl 5 !
(!) :: Markup -> Primitive -> Markup
(!) = flip (:)

infixl 5 !:
(!:) :: Markup -> DString -> Markup
m !: v = m ! Content v

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
                       else error "Markup has more close elements than open"
    where n = foldr nestLevel 0 m

at :: DString -> DString -> Primitive
at = Attribute

atN :: DString -> a -> Primitive
atN t v = Attribute t (show v)

atP :: DString -> Double -> Primitive
atP t v = Attribute t (show v ++ "%")

co :: DString -> Primitive
co = Content

el :: DString -> Primitive
el = Element

pretty :: Markup -> String
pretty m = concat $ zipWith cat ins rm
    where rm = reverse m
          ins = scanl (flip nestLevel) 0 rm
          cat i x = replicate (i * 4) ' ' ++ go x ++ "\n"
          go (Element   x)   = "<" ++ x ++ ">"
          go (Attribute x y) = x ++ "=\"" ++ y ++ "\""
          go (Content   x)   = "\"" ++ x ++ "\""
          go (Complete)      = "<--"

nestLevel :: Primitive -> Int -> Int
nestLevel (Element _) x = x + 1
nestLevel (Complete)  x = x - 1
nestLevel _           x = x
