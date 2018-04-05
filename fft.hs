{-# OPTIONS_GHC -O2 #-}

{- |
  Module      :  Numeric.FFT
  Copyright   :  (c) Matt Morrow 2008
  License     :  BSD3
  Maintainer  :  Matt Morrow <mjm2002@gmail.com>
  Stability   :  experimental
  Portability :  portable


  A radix-2 DIT version of
  the Cooley-Tukey FFT algorithm.
-}

module Numeric.FFT (
    fft, ifft
  , dft, idft
) where

import Data.List(foldl')
import Data.Complex(Complex(..))

-----------------------------------------------------------------------------

-- | /O(n lg n)/. A radix-2 DIT
--  (decimation-in-time) version of the
--  Cooley-Tukey FFT algorithm.
--  The length of the input list /must/
--  be a power of two, or only the prefix
--  of the list of length equal to the largest
--  power of two less than the length of the list
--  will be transformed.

:{
let fft :: [Complex Double] -> [Complex Double]
    fft []    = []
    fft [x,y] = dft [x,y]
    fft xs    = go len ys zs [0..len*2-1]
      where (ys,zs) = (fft***fft)
                . deinterleave $ xs
            len = length ys
            go len xs ys ks = zipWith (flip ($)) (ys ++ ys)
                                (zipWith (flip ($)) (xs ++ xs)
                                  (fmap (f len) ks ++ fmap (g len) ks))
            f len k x y = x + y * exp (negate(2*pi*i*fi k)/fi(len*2))
            g len k x y = x - y * exp (negate(2*pi*i*fi k)/fi(len*2))
            (***) f g (x,y) = (f x, g y)
            deinterleave :: [a] -> ([a],[a])
            deinterleave = unzip . pairs
            pairs :: [a] -> [(a,a)]
            pairs []       = []
            pairs (x:y:zs) = (x,y) : pairs zs
            pairs _        = []
            fi = fromIntegral
            i = 0 :+ 1
:}


(3 :+ 3) + (4:+ 5)

fft [(3 :+ 3),(4:+ 5)]

fft [(12 :+ 3),(3 :+ 234),(3 :+ 3),(4:+ 5)]

-- only works for inputs with length = 2^n for n int
fft [(3 :+ 234),(3 :+ 3),(4:+ 5)]

ifft :: [Complex Double] -> [Complex Double]
ifft xs = let n = (fromIntegral . length) xs in fmap (/n) (fft xs)

-----------------------------------------------------------------------------

-- | /O(n^2)/. The Discrete Fourier Transform.

:{
let dft :: [Complex Double] -> [Complex Double]
    dft xs = let len = length xs
              in zipWith (go len) [0..len-1] (repeat xs)
      where i = 0 :+ 1
            fi = fromIntegral
            go len k xs = foldl' (+) 0 . flip fmap [0..len-1]
              $ \n -> (xs!!n) * exp (negate(2*pi*i*fi n*fi k)/fi len)
:}

idft :: [Complex Double] -> [Complex Double]
idft xs = let n = (fromIntegral . length) xs in fmap (/n) (dft xs)

-----------------------------------------------------------------------------

