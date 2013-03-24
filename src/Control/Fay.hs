module Control.Fay
    (
    ap
    ,mapM
    ,foldM
    ,zipWithM
    ,zipWithM_
    ,replicateM
    )
    where

import Prelude
import FFI

ap :: Fay (a -> b) -> Fay a -> Fay b
ap f x = f >>= \f' -> x >>= \x' -> return (f' x')

mapM :: (a -> Fay b) -> [a] -> Fay [b]
mapM f as = sequence (map f as)

foldM :: (a -> b -> Fay a) -> a -> [b] -> Fay a
foldM _ x [] = return x
foldM f y (x:xs) = f y x >>= \z -> foldM f z xs

zipWithM :: (a -> b -> Fay c) -> [a] -> [b] -> Fay [c]
zipWithM f xs ys = sequence (zipWith f xs ys)

zipWithM_ :: (a -> b -> Fay c) -> [a] -> [b] -> Fay ()
zipWithM_ f xs ys = sequence_ (zipWith f xs ys)

replicateM :: Int -> Fay a -> Fay [a]
replicateM n x = sequence (replicate n x)
