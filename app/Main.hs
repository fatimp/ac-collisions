module Main where
import Polynomial.Random
import Polynomial.Symmetry
import System.Random
import System.Environment
import Factor.Zx
import Control.Monad
import System.IO

data Report = Report {processed :: Int, found :: Int} deriving Show

initialReport :: Report
initialReport = Report 0 0

analizePolynomial :: Report -> Zx -> (Report, Bool)
analizePolynomial rep poly = (rep {processed = processed rep + 1,
                                   found = found rep + if reporting then 1 else 0},
                              reporting) where
  reporting = countNonSymFactors poly > 1

report :: Report -> Zx -> IO Report
report rep polynomial = do
  let (newrep, reporting) = analizePolynomial rep polynomial
  when reporting $ do
    hPutStrLn stdout "Collision found:"
    forM_ (collisions polynomial) $ hPrint stdout
  when (rem (processed newrep) 100 == 0) $ hPrint stderr newrep
  pure newrep

main :: IO ()
main = do
  args <- getArgs
  gen  <- initStdGen
  when (length args /= 1) $ error "Usage: check-random-polys deg"

  let polys = randomPolynomials (read $ head args) gen
  foldM_ report initialReport polys
