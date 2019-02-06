#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

import CPVO.Numeric
import CPVO.IO
import CPVO.IO.Reader.Ecalj.Common
import CPVO.IO.Reader.Ecalj.DOS






import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List
-------------------------

import Numeric.LinearAlgebra
-------------------------

rydberg :: Double
rydberg=1/13.605

testArgs :: [String]
testArgs = concat [ [ "GWtable01.tex"
                   , "Total and partial DOS at *E*~F~ in states/eV/unit-cell, QSGW method."
                   , "|*N*$_\\uparrow$(*E*~F~)|*N*$_\\downarrow$(*E*~F~)|*N*(*E*~F~)"
                     ]
                   , words "@{}lSSS@{} -9:6 25 T o flipSpin nico2o4 extendedNiCo2O4.normal/nico2o4.6G10 Ni:Ni#3d:6:7:9:8:10 Co:Co#3d:6:7:8:9:10 O:O#2p:3:4:5"
                   ]

main :: IO ()
main = do
    allArgs <- getArgs
    --Right (invStat, ymax, xmin, xmax, ctrlAtoms, uniqAtoms, ctrlAtomicAOs,jdTable, jdHeads, foldernya, tailer, colAlign, texFile) <- readHeaderData allArgs
    Right (invStat,_,_,_,_,_, ctrlAtomicAOs,jdTable, jdHeads, foldernya, tailer,_, texFile) <- readHeaderData allArgs
    totalDOS <- readTotalDOSText tailer foldernya
    let resTotY0 = map (\i -> getY0 $ totalDOS ¿ [0,i]) $ flipBy invStat [1,2]
    putStrLn "===========NF total DOS========="
    putStrLn $ show resTotY0
    pdosAtAll <- readPDOS invStat tailer foldernya ctrlAtomicAOs
    let rs' = rendertable
              $ (:) jdHeads
              $ (:)  ("Total" : (map (showDouble 3) $ processCols $ map ((* rydberg) . last . toList) $ resTotY0))
              $ map (\a -> (((fst $ head a):(map (showDouble 3) $ processCols $ sumIt [0,0] $ map snd a))))
              $ groupBy (\a b -> fst a == fst b)
              $ (\[us,ds] -> zipWith (\(yu,(_,(j,_))) (yd,_) -> (j, [yu,yd])) us ds )
              $ groupBy (\(_,(s,_)) (_,(s',_)) ->  s == s')
              $ map (\(mp,b) -> ((* rydberg) $ last $ toList $ getY0 mp, b)) pdosAtAll
    let rs = unlines [ rs' , jdTable ]
    T.writeFile texFile $ T.pack rs
{-
    res' <- markdownToTex rs
    let res = T.replace "\\}" "}"
            $ T.replace "\\{" "{"
            $ T.pack $ unlines  [ "\\begin{longtable}[]{" ++ colAlign ++ "}"
                                , unlines $ tail $ lines $ T.unpack res'
                                ]
    T.writeFile texFile res
    -}
    putStrLn $ texFile ++ "=============done:t1dosEf.hs=========="
      where
        processCols [u,d] = [u,d,u+d]
        processCols _ = []
        sumIt x [] = x
        sumIt [u0,d0] ([up,dn]:xs) = sumIt [u0+up,d0+dn] xs
        sumIt _ _ = []

