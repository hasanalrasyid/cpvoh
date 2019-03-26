#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

--import Data.Maybe
--import Data.Char (ord)
--import Data.List.Split
--import Control.Monad.IO.Class (MonadIO)
import Data.Either (rights)

import CPVO.Numeric
import CPVO.IO
import CPVO.IO.Type
import CPVO.IO.Reader.Util
import CPVO.IO.Reader.Ecalj.Util
import CPVO.IO.Reader.Ecalj.DOS


--import qualified System.Process as SP
--import Text.Printf as TP
--import System.IO (openTempFile,hClose)




import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List
-------------------------

import Numeric.LinearAlgebra hiding (find)
-------------------------

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
    Right aa@(invStat,_,_,_,_,_, ctrlAtomicAOs,jdTable, jdHeads, foldernya, tailer,_, texFile,targetPrint) <- readHeaderData allArgs
    putStrLn $ "===========allArgs =========" ++ show aa
    putStrLn $ "===========targetPrint =========" ++ show targetPrint
    totalDOS <- readTotalDOSText tailer foldernya
    let resTotY0 = map (\i -> getY0 $ totalDOS ¿ [0,i]) $ flipBy invStat [1,2]
    putStrLn "===========NF total DOS========="
    debugIt "ctrlAtomicAOs=========" ctrlAtomicAOs
    putStrLn $ show resTotY0
    pdosAtAll <- readPDOS invStat tailer foldernya
                  [ Cetak s a | s <- flipBy invStat [1,2], a <- ctrlAtomicAOs ]
    --putStrLn $ "===pdosAtAll " ++ show pdosAtAll
    putStrLn "=========!readPDOS"
    let rs' = rendertable
              $ (:) jdHeads
              $ (:)  ("Total" : (map (showDouble 3) $ processCols $ map ((* fromRydberg) . last . toList) $ resTotY0))
              $ map go5
              $ map (sumIt (ErrAtOrb,0,0))
              $ groupBy (\(a,_,_) (b,_,_) -> sameLabelOfAtoms a b)
              $ sortBy (\(a,_,_) (b,_,_) -> compareLabelOfAtoms a b)
              $ map go4
              $ groupBy sameAtomofCetaks
              $ sortBy (\x y -> compare (atom $ snd x) (atom $ snd y))
              $ map (\(mp,b) -> ((* fromRydberg) $ last $ toList $ getY0 mp, b)) pdosAtAll
    putStrLn $ unlines $ "====rs ":rs':[]
    let rs = unlines $ rs':jdTable:[]
    T.writeFile texFile $ T.pack rs
      where
        go5 (a,u,d) = (labelAO a):(map (showDouble 3) $ sumUpDown u d)
        sumIt (_,r1,r2) ((a,b,c):as) = sumIt (a,r1+b,r2+c) as
        sumIt res [] = res
        compareLabelOfAtoms a b = compare (labelAO a) (labelAO b)
        sameLabelOfAtoms a b = labelAO a == labelAO b
        sameAtomofCetaks (_,(Cetak _ a)) (_,(Cetak _ b)) = a == b
        sameAtomofCetaks _ _ = False
        go4 ((u,Cetak _ a):(d,_):_) = (a,u,d)
        go4 _ = (ErrAtOrb,9999,9999)
        go3 ((l,(_,u)):(_,(_,d)):_) = (l:(map (showDouble 3) $ processCols [u,d]))
        go3 _ = "wrongFormGO3":[]
        go2 = groupBy (\(_,s) (_,s') ->  (labelAO $ atom s) == (labelAO $ atom s'))
        go :: [(Double,Cetak)] -> Either String (String,(SpinID,Double))
        go a@((_, (Cetak s (AO _ _ labelAt _))):_) = Right (labelAt,(s, sum $ map fst a))
        go _ = Left "wrongFormGO"
        sumUpDown u d = [u,d,u+d]
        processCols [u,d] = [u,d,u+d]
        processCols _ = []

