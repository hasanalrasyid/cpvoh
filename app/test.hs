{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Language.C.Inline as C
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import           Data.Monoid ((<>))
import           Foreign.C.Types

import CPVO.IO
import CPVO.Numeric
import CPVO.IO.DOS
import CPVO.IO.Reader.Parser
import CPVO.IO.Reader.Ecalj.Band
import CPVO.IO.Reader.Ecalj.DOS
import CPVO.IO.Reader.Ecalj.MMOM
import CPVO.IO.Reader.Ecalj.Util
import CPVO.IO.Reader.Util
import CPVO.IO.Plot.Band
import CPVO.IO.Plot.Gnuplot.DOS
import CPVO.IO.Plot.Gnuplot.Common
import CPVO.IO.Plot.Gnuplot.Type
import CPVO.IO.Plot.DOS
import CPVO.IO.Fortran
import CPVO.IO.Type
import CPVO.Data
import Test
-------------------------

foreign import ccall safe "add" f_add :: IO ()

C.context (C.baseCtx Data.Monoid.<> C.vecCtx)
C.include "<stdio.h>"
C.include "<math.h>"
C.verbatim "extern double add_ (int *, double [], int *, double []);"

test1 :: CInt -> IO ()
test1 n = do
  s <- readAndSum n
  [C.exp| void{ printf( "SuminC: %.2d\n", $(int s)) } |]
  putStrLn $ "Sum: " ++ show s

readAndSum :: CInt -> IO CInt
readAndSum n =
  [C.block| int {
      int i, sum = 0, tmp;
      for (i = 0; i < $(int n); i++) {
        scanf("%d ", &tmp);
        sum += tmp;
      }
      return sum;
    } |]


sumVec :: VM.IOVector CDouble -> IO CDouble
sumVec vec = [C.block| double {
    double sum = 0;
    int i;
    for (i = 0; i < $vec-len:vec; i++) {
      sum += $vec-ptr:(double *vec)[i];
    }
    int x, y;
    double z;

    x = 3;
    y = 3;


    z = add_ (&x, $vec-ptr:(double *vec), &y, $vec-ptr:(double *vec)); /* Call Fortran add routine */
    /* Note: Fortran indexes arrays 1..n */
    /* C indexes arrays 0..(n-1) */

    printf("The sum of %1.0f and %1.0f is %2.0f \n",
    $vec-ptr:(double *vec)[x-1], $vec-ptr:(double *vec)[y-1], z);
    return sum;
  } |]

main :: IO ()
main = do
  putStrLn "=======start: Test========="
  -- V.thaw will copy the vector
  -- V.unsafeThaw will not copy the vector
  x <- sumVec =<< V.unsafeThaw (V.fromList $ take 5 [1,2..])
  print x
  putStrLn "=======end  : Test========="

rydberg :: Double
rydberg=1/13.605

testArgs :: [String]
testArgs = concat [ [ "GWtable01.tex"
                   , "Total and partial DOS at *E*~F~ in states/eV/unit-cell, QSGW method."
                   , "|*N*$_\\uparrow$(*E*~F~)|*N*$_\\downarrow$(*E*~F~)|*N*(*E*~F~)"
                     ]
                   , words "@{}lSSS@{} -9:6 25 T o flipSpin nico2o4 extendedNiCo2O4.normal/nico2o4.6G10 Ni:Ni#3d:6:7:9:8:10 Co:Co#3d:6:7:8:9:10 O:O#2p:3:4:5"
                   ]

{-
main1 = do
--    allArgs <- getArgs
    (invStat, ymax, xmin, xmax, ctrlAtoms, uniqAtoms, ctrlAtomicAOs,jdTable, jdHeads, foldernya, tailer, colAlign, texFile) <- readHeaderData testArgs
    totalDOS <- readTotalDOSText tailer foldernya
    let resTotY0 = map (\i -> getY0 $ totalDOS ¿ [0,i]) $ flipBy invStat [1,2]
    putStrLn "===========NF total DOS========="
    putStrLn $ show resTotY0
    pdosAtAll <- readPDOS invStat tailer foldernya ctrlAtomicAOs
    let y0AtAll = map (\(mp,b) -> (getY0 mp,b)) pdosAtAll
    let rs' = rendertable
              $ (:) jdHeads
              $ (:)  ("Total" : (map (showDouble 3) $ processCols $ map ((* rydberg) . last . toList) $ resTotY0))
              $ map (\a -> (((fst $ head a):(map (showDouble 3) $ processCols $ sumIt [0,0] $ map snd a))))
              $ groupBy (\a b -> fst a == fst b)
              $ (\[us,ds] -> zipWith (\(yu,(_,(j,_))) (yd,_) -> (j, [yu,yd])) us ds )
              $ groupBy (\(_,(s,_)) (_,(s',_)) ->  s == s')
              $ map (\(mp,b) -> ((* rydberg) $ last $ toList $ getY0 mp, b)) pdosAtAll
    let rs = unlines [ rs' , jdTable ]
    res' <- markdownToTex rs
    let res = T.replace "\\}" "}"
            $ T.replace "\\{" "{"
            $ T.pack $ unlines  [ "\\begin{longtable}[]{" ++ colAlign ++ "}"
                                , unlines $ tail $ lines $ T.unpack res'
                                ]
    T.writeFile texFile res
    putStrLn $ texFile ++ "=============done:t1dosEf.hs=========="
      where
        processCols [u,d] = [u,d,u+d]
        sumIt x [] = x
        sumIt [u0,d0] ([up,dn]:xs) = sumIt [u0+up,d0+dn] xs
-}
