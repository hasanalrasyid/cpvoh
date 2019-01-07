{-# LANGUAGE OverloadedStrings #-}

module Main where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.HMatrix

import System.Environment
import Data.List (groupBy,partition)
import Data.Attoparsec.Text
import Data.Text (pack)

import CPVO.Data
import CPVO.Parser



main :: IO ()
main = do
  aParam':cParam':nMESH':_ <- getArgs
  if nMESH' == [] then runHelp
                  else calcMesh aParam' cParam' nMESH'

runHelp = do
  putStrLn "runit using : ./exe aParam cParam nMESH"
  putStrLn "ex: ./exe 3.25 2.55 5"


calcMesh aParam' cParam' nMESH' = do
  let nMESH = read nMESH' :: Double
  let aParam = read aParam' :: Double
  let cParam = read cParam' :: Double
  let gmk = (3><3) [  0,    0,    0,
                      0.5,  0,    0,
                      1/3,  1/3,  0 ] :: Matrix Double
  let [gamma,mnya,knya] = toRows gmk

  let latticeVectorMatrix = (3><3) [ 0,1,1,  1,0,1, 1,1,0 ] :: Matrix Double
  let bMatrix = inv latticeVectorMatrix

  let dM = scale (1/nMESH )  (mnya - gamma )
  let dMK = scale (1/nMESH )  (knya - mnya )

  disp 6 $ (\x -> mul x bMatrix ) $ fromRows $ concat $ map  (tpq gamma dM dMK ) [0..nMESH]

tpq gamma dM dMK p =  map (\q -> (gamma + (scale p dM) ) + (scale q  dMK)) ([0..p])

hasil vektornya dnya n = vektornya + (scale n dnya  )
