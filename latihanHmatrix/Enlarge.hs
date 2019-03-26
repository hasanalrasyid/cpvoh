{-# LANGUAGE OverloadedStrings #-}

module Main where

import Numeric.LinearAlgebra
--import Numeric.LinearAlgebra.Data
--import System.Environment
--import Text.Printf (printf)
import Data.List (groupBy,partition)
import Data.Attoparsec.Text 
import Data.Text (pack)
--import Data.Either
--import System.Environment

import CPVO.Data
import CPVO.Parser



main :: IO ()
main = do
--  [f1] <- getArgs
--  vCF1s <- loadMatrix f1
  x <- readFile "case/sample.dat"
  x' <- readFile "case/celldm"
  let Right (ibrav, celldms) = parseOnly parseCellParam $ pack x'
  let grupnya = partition (\xx -> (elem '.' . head .head ) xx)
        $ groupBy (\u v ->(elem '.' . head) u == (elem '.' . head) v ) $ map words $ lines x
  let fCoord = parseOneRow parseCoord $ fst grupnya
  let fAtom = parseOneRow parseAtom $ snd grupnya
  let crystal  = Crystal ibrav celldms (matrix 1 [1])  (matrix 1 [1]) $ zipWith (\a b -> Atoms (head a) b) fAtom fCoord
  putStrLn $ show crystal



  
--  let step = read st :: Double
--  let [vCoord1,vForce1] = map (\x -> subMatrix (0,x) (rows vCF1s,3) vCF1s) [0,3]
--  let vX =  vCoord1 + (scale step vForce1)
--  putStrLn $ format " " (printf "%.6f") crystal 
