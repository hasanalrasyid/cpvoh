module Main where

--import Lib
import CPVO
import CPVO.Parser
import CPVO.Data
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import System.Environment
import Text.Printf (printf)
import Data.List

import Control.Monad (replicateM, when)
import Data.Traversable (for)
import System.Environment (getArgs)

main = do

  -- numb:wq
  -- ers we are looking for
  numbers <- getArgs 

  -- get the key-value metadata
  metadata <- replicateM 6 $ do
    [key,value] <- words <$> getLine
    return (key,value)

  let Just rows = read <$> lookup "nrows" metadata
      Just cols = read <$> lookup "ncols" metadata

  -- loop over all the entries
  for [1..rows] $ \row ->do
    rawRow <- words <$> getLine
    for (zip [1..cols] rawRow) $ \(col,cell) ->
      when (cell `elem` numbers)
        (putStrLn ("The value " ++ cell ++ " is in row " ++ show row ++ " and column " ++ show col))
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
  for i in -1,0,1; do
    for j in -1,0,1; do
    echo $i $j
  done
  
 FINAL GENERATOR 
 fromLists $    -- generate matrix dari list
    map fst $  -- ambil a dari (a,b)
    nubBy (\a b -> snd a == snd b ) $  -- hapus anggota list memiliki b yang sama
    map (\y -> (y,takeDiag  $                                 -- dari setiap anggota list,  buat pair (y,x) dimana x adalah diagonal dari yTranspose <> y (x_ij = (y_ij)^2)
                    (tr $fromLists [y]) <> fromLists [y] ) )  --tapi ini tampaknya bisa disederhanakan menjadi
    map (\y -> (y, map (^2) ))
    [[i,j,k] | i <- [-1..1], j <- [-1..1],k <- [-1..1] ]

 perintah:
stack exec exe | awk '{print "mkdir -p ni3o4."$1$2$3""}'              
stack exec exe| awk '{print "sed -e \"s/xxnt1/"$1"/g\" -e \"s/xxnt2/"$2"/g\" -e \"s/xxnio/"$3"/g\" template/ctrl.ni3o4 > ni3o4."$1$2$3"/ctrl.ni3o4"}'
for i in ni3o4.*; do cd $i; lmchk --pr60 ni3o4; lmfa ni3o4 > llmfa; cd ..;done                           

jcf.sekirei.sh

jcf.hakozaki.sh
for i in $(ls |grep ni3o4 |sort -r ); do
cd $i
mpiexec -machinefile $PBS_NODEFILE -np 24 $EXEPATH/lmf-MPIK ni3o4 > llmf
cd ..
done
--}

--susunHasil :: [[Double]] -> Matrix Double
--susunHasil x = 
main :: IO ()
main = do
  let daftarLengkap = [[i,j,k] | i <- [-1..1], j <- [-1..1],k <- [-1..1] ] :: [[Double]]
--  let hasil = daftarLengkap
{-
  let hasil = fromLists $    -- generate matrix dari list
              map fst $  -- ambil a dari (a,b)
              nubBy (\a b -> (snd a) == (snd b) ) $  -- hapus anggota list memiliki b yang sama
--              map (\y -> (y,takeDiag  $                                 -- dari setiap anggota list,  buat pair (y,x) dimana x adalah diagonal dari yTranspose <> y (x_ij = (y_ij)^2)
--                (tr $fromLists [y]) <> fromLists [y] ) )  --tapi ini tampaknya bisa disederhanakan menjadi
              map (\y -> (y, map (^2) y )) daftarLengkap 
              -}
  let hasil = fromRows $ nubBy (\a b ->  scale (-1) a == b) $ map fromList daftarLengkap 
--  putStrLn $ show hasil
  putStrLn $ format "  " (printf "%.0f") hasil
--  putStrLn $ format "  " (printf "%.5f") (hasil :: Matrix Double)





--newtype Moment = Vector 
--data MMagnet 

-- fromRows  $ map (fst) $ nubBy (\a b -> snd a == snd b ) $ map (\(z,y) ->
-- (z,takeDiag  $ (tr y) <> y ) ) $ map (\x -> (x,fromRows [x])) $ toRows
-- daftarArah
--


-- fromRows  $ map (fst) $ nubBy (\a b -> snd a == snd b ) $ map (\(z,y) ->
-- (z,takeDiag  $ (tr y) <> y ) ) $ map (\x -> (x,fromRows [x])) $ toRows
-- daftarArah 
--
--
{-
main :: IO ()
main = do
  (f1:st) <- getArgs
  vCF1s <- loadMatrix f1
  let arah = 4 |>  (map read st :: [Double])
  let daftarArahM = (8><4) [  0,0,0,0
                   ,  0,-1,0,0
                   ,  0,0,-1,0
                   ,  0,-1,-1,0
                   ,  0,-1,-1,-1
                   ,  0,-1,0,-1
                   ,  0,0,-1,-1
                   ,  0,-1,-1,-1
                   ] :: Matrix Double
  let vCoord1 = vCF1s
      arahM = repmat (row $ toList arah) (rows vCoord1) 1
--      vX = vCoord1 - arahM
--      vX = fromRows $ map (\xx -> vCoord1 - (repmat (asRow xx) (rows vCoord1) 1)) $ toRows daftarArahM
      vX = foldr (===) (fromLists [[0,0,0,0]]) $  map (\xx -> (vCoord1 - (repmat (asRow  xx) (rows vCoord1) 1))/2 ) $ toRows daftarArahM
--      vX =  fromRows $ map (\x -> x - arah) $ toRows vCoord1 
--  let vX =  fromRows $ map (\xx -> fromRows $ map (\x -> x - xx) $ toRows vCoord1 ) daftarArah
  putStrLn $ format "  " (printf "%.5f") vX
  -}
