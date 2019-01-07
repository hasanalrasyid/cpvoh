module Main where

import Lib
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import System.Environment
import Text.Printf (printf)
{-- failed 
main :: IO ()
main = do
  [f1,f2] <- getArgs
  vCF1s <- loadMatrix f1
  vCF2s <- loadMatrix f2
  let [vCoord1,vCoord2] = map (subMatrix (0,0) (rows vCF1s,3)) [vCF1s,vCF2s]
  let [vForce1,vForce2] = map (subMatrix (0,3) (rows vCF1s,3)) [vCF1s,vCF2s]  
  let vX =  vCoord1 - ((vCoord2-vCoord1)/(vForce2-vForce1))*vForce1*1000
  print vX
  
  
--}
main :: IO ()
main = do
  [f1,st] <- getArgs
  vCF1s <- loadMatrix f1
  let step = read st :: Double
  let [vCoord1,vForce1] = map (\x -> subMatrix (0,x) (rows vCF1s,3) vCF1s) [0,3]
  let vX =  vCoord1 + (scale step vForce1)
  putStrLn $ format " " (printf "%.6f") vX
