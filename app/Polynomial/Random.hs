module Polynomial.Random (randomPolynomials) where

import System.Random
import Control.Monad.State.Strict
import Factor.Zx
import Data.List as L
import Data.IntMap.Strict

randomDigits :: RandomGen a => Int -> a -> ([Integer], a)
randomDigits n gen = (`runState` gen) $ sequence $ replicate n $ do
  oldgen <- get
  let (digit, newgen) = uniformR (0, 1) oldgen
  put newgen
  return digit

coeffToPolynomial :: [Integer] -> Zx
coeffToPolynomial = fromNormCoeffMap . fromAscList . L.filter nonZero . zipWith (,) [0..] where
  nonZero (_, coeff) = coeff /= 0

randomPolynomial :: RandomGen a => Int -> a -> (Zx, a)
randomPolynomial deg gen = let (coeff, newgen) = randomDigits (deg - 1) gen in
  (coeffToPolynomial ([1] ++ coeff ++ [1]), newgen)

-- Generate infinite list of random polynomials of degree `deg` with free term = 1 
randomPolynomials :: RandomGen a => Int -> a -> [Zx]
randomPolynomials deg = unfoldr $ Just . randomPolynomial deg
