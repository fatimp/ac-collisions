{-# LANGUAGE BangPatterns #-}

module Polynomial.Symmetry (countNonSymFactors, collisions) where

import Factor.Zx
import Factor.Bz
import Data.List as L
import Data.IntMap.Strict

-- Transfrom polynomial f to x^{deg f} * f(x^-1)
reversePolynomial :: Zx -> Zx
reversePolynomial = fromNormCoeffMap . (mapKeys <$> keymod <*> coeffMap) where
  keymod poly d = degree poly - d

-- Check is f and its reverse are equal
isSymmetric :: Zx -> Bool
isSymmetric = (==) <*> reversePolynomial

-- The same as Bz.factor, but represent factors f^n as f repeated n times
collectFactors :: Zx -> [Zx]
collectFactors poly = do
  (f, power) <- snd $ factor poly
  replicate (fromInteger power) f

-- Count a number of non-symmetrical factors in f
countNonSymFactors :: Zx -> Int
countNonSymFactors = length . L.filter (not . isSymmetric) . collectFactors

-- Represent a polynomial f as (m, fs) where m is a product of all symmetrical factors
-- and fs is a list of all non-symmetrical factors
expandSymmetrical :: Zx -> (Zx, [Zx])
expandSymmetrical = expand' ((fromMonomial $ Monomial 0 1), []) . collectFactors where
  expand' (!m, !fs) (x:xs) = if isSymmetric x then
    expand' ((multiply m x), fs) xs else
    expand' (m, x:fs) xs
  expand' res [] = res

-- Reduce the degree of freedom in `expandSymmetrical` representation by 1.
multiplyOne :: (Zx, [Zx]) -> (Zx, [Zx])
multiplyOne (!m, !(f:fs)) = ((f `multiply` m), fs)
multiplyOne (m, []) = (m, [])

-- For a polynomial f with free term = 1, return all polynomials which
-- have the same degree and autocorrelation as f (with exception of simple reversions)
collisions :: Zx -> [Zx]
collisions poly = L.map (L.foldl1' multiply) $ L.foldr (<*>) [[m]] pairs where
  (m, fs) = multiplyOne $ expandSymmetrical poly
  pairs = L.map (\f -> [(f:), (reversePolynomial f:)]) fs
