module Main where
import Polynomial.Random
import Polynomial.Symmetry
import Polynomial.Utility
import System.Random
import System.Environment
import Factor.Zx
import Control.Monad
import System.IO

data Report = Report {processed :: Int, found :: Int} deriving Show

initialReport :: Report
initialReport = Report 0 0

report' :: Zx -> IO ()
report' poly = do
  hPutStrLn stdout "Collision found:"
  forM_ (collisions poly) $ hPrint stdout
  hPutStrLn stdout "As Lisp bit vectors:"
  forM_ (collisions poly) $ hPutStrLn stdout . polyAsLispValue
  hPutStrLn stdout "As Julia vectors:"
  forM_ (collisions poly) $ hPutStrLn stdout . polyAsJuliaValue
  hPutStrLn stdout "Factors:"
  hPutStrLn stdout $ showFactors poly

report :: Report -> Zx -> IO Report
report rep polynomial = do
  let reporting = countNonSymFactors polynomial > 1
  let newrep = rep {
        processed = processed rep + 1,
          found = found rep + if reporting then 1 else 0
        }
  when reporting $ report' polynomial
  when (rem (processed newrep) 100 == 0) $ hPrint stderr newrep
  pure newrep

main :: IO ()
main = do
  args <- getArgs
  gen  <- initStdGen
  when (length args /= 1) $ error "Usage: check-random-polys deg"

  let polys = randomPolynomials (read $ head args) gen
  foldM_ report initialReport polys
