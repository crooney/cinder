{-# LANGUAGE NoImplicitPrelude #-}
module Control.Fay
    (
    ap
    ,foldM
    ,zipWithM
    ,zipWithM_
    ,replicateM
    )
    where

import           FFI
import           Prelude hiding (mapM)

ap :: Fay (a -> b) -> Fay a -> Fay b
ap f x = f >>= \f' -> x >>= \x' -> return (f' x')

foldM :: (a -> b -> Fay a) -> a -> [b] -> Fay a
foldM _ x [] = return x
foldM f y (x:xs) = f y x >>= \z -> foldM f z xs

zipWithM :: (a -> b -> Fay c) -> [a] -> [b] -> Fay [c]
zipWithM = ((sequence .) .) . zipWith

zipWithM_ :: (a -> b -> Fay c) -> [a] -> [b] -> Fay ()
zipWithM_  = ((sequence_ .) .) . zipWith

replicateM :: Int -> Fay a -> Fay [a]
replicateM = (sequence .) . replicate
