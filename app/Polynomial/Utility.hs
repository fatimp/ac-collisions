module Polynomial.Utility (showFactors, polyAsLispValue, polyAsJuliaValue) where

import Factor.Zx
import Factor.Bz
import Data.List as L
import Data.IntMap.Strict

showFactors :: Zx -> String
showFactors poly = case factor poly of
  (_, factors) -> intercalate " " $ L.map showFactor factors where
    showFactor (poly', 1)     = "(" ++ show poly' ++ ")"
    showFactor (poly', power) = "(" ++ show poly' ++ ")^" ++ show power

polyCoeff :: Zx -> [Integer]
polyCoeff poly = L.map query [0..(degree poly)] where
  query n = findWithDefault 0 n coeffs
  coeffs = coeffMap poly

polyAsLispValue :: Zx -> String
polyAsLispValue = ("#*"++) . concat . L.map show . polyCoeff

polyAsJuliaValue :: Zx -> String
polyAsJuliaValue = show . polyCoeff
