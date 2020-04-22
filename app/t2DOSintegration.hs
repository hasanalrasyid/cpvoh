#!/usr/bin/env stack
--stack --resolver lts-14.27 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

--import CPVO.IO.DOS(showTotPDOS)

import System.Environment (getArgs)

import CPVO.IO.Reader.Ecalj.Util
import CPVO.IO.Reader.Ecalj.DOS
import CPVO.Numeric
import Numeric.LinearAlgebra
import CPVO.IO
import CPVO.IO.Type
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  allArgs <- getArgs
  putStrLn $ show allArgs
  {-
  let testArgs = [ "table02.tex"
                 ,"Atomic magnetic moment"
                 ,"{Atom Number}|{Atomic Orbital}|{Spin Density Integration}|{PDOS Integration}|{Difference}"
                 -- column alignment options, sumthing like
                 -- @{}lSSS@{} in \begin{longtable}[]{@{}lSSS@{}}
                 , "@{}lSSS@{}"
                 ,"-9:6","25","T","o","1","nico2o4"
                 ,"extendedNiCo2O4.normal/nico2o4.0GGA"
                 ,"Ni:Ni#3d:6:7:9:8:10","Co:Co#3d:6:7:8:9:10","O:O#2p:3:4:5" ]
  -}
  showTotPDOS allArgs
  putStrLn "========beres======="

--import CPVO.Numeric
--import CPVO.IO
--import CPVO.IO.Type
--import CPVO.IO.Reader.Ecalj.DOS

--import Data.List
--import Numeric.LinearAlgebra

genN_EF ((_:a@(a1:_)):(_:b@(b2:_)):_) =
  if a1 * b2 < 0 then
                   takeMiddle a b -- zero is between them both, then take the middle
                 else a -- zero is at the smallest, means s

takeMiddle (a:l1) (b:l2) = let f  = abs $ a / (b - a)
                               getMid fa (u1,u2) = u1 + (fa * (u2 - u1))
                            in (map (getMid f) $ zip l1 l2)

takeN_Ef d = genN_EF $ take 2 $ sortOn head $ map (\l@(a:_) -> (a*a):l) $ filter (\(a:_) -> a*a < 0.00001) $ toLists d

atnumFromCetak = atnum . atom . snd

diffCetak ((i1, Cetak 1 a):(i2,Cetak 2 _):_) = (i1 - i2, a)
diffCetak ((i2, Cetak 2 a):(i1,Cetak 1 _):_) = (i2 - i1, a)
diffCetak _ = (0, ErrAtOrb)

showTotPDOS :: [String] -> IO ()
showTotPDOS allArgs = do
--  (invStat, ymax, xmin, xmax, ctrlAtoms, uniqAtoms, ctrlAtomicAOs,jdTable, cleanedJdHead, foldernya, tailer, colAlign, texFile) <- readHeaderData allArgs
  Right (invStat, _ , _ , _ , _ , _ , ctrlAtomicAOs,jdTable, cleanedJdHead, foldernya, tailer, _ , texFile,_) <- readHeaderData allArgs

  -------------------------------generating DOS data------------------------
  totalDOS <- readTotalDOSText tailer foldernya
  -------------------------------integrating DOS data------------------------
  let intgTot = map (\i -> integrateToZero $ totalDOS ¿ [0,i]) $ flipBy invStat [1,2]
  pdosAtomicAll <- readPDOS invStat tailer foldernya [ Cetak s a | s <- flipBy invStat [1,2], a <- ctrlAtomicAOs ]
    {-
  putStrLn $ "===" ++ (show $ takeN_Ef totalDOS)
  let valN_Ef = sortOn (atnum . snd)
                $ map diffCetak
                $ groupBy (\a b -> atnumFromCetak a == atnumFromCetak b)
                $ sortOn atnumFromCetak
                $ map (\(d,x) -> (head $ takeN_Ef d ,x)) $ pdosAtomicAll
  putStrLn $ "===" ++ (show valN_Ef)
  -}

  let integratedAtPDOS = integrateAtomicPDOS pdosAtomicAll
  let rIntgAll' = id -- $ show
                $ rendertable
                $ (:) cleanedJdHead
                $ (:) (concat [["Total"]
                             , map (showDouble (3::Integer)) $ (\[u,d] -> [u,d,u-d,u+d]) $ intgTot
                             ]
                      )
                $ (\(u:d:_) -> zipWith (\(s1,at) (s2,_) -> ((labelAO $ atom at):map (showDouble (3::Integer)) [s1,s2,s1-s2,s1+s2])) u d)
                $ map (\m   -> map (\l -> ((sum $ map fst l), snd $ head l))
                            $ groupBy (\(_,Cetak _ a) (_,Cetak _ b) -> labelAO a == labelAO b) m)
--                $ head
                $ map (sortOn atnumFromCetak)
                $ groupBy (\(_,a) (_,b) -> spinID a == spinID b)
                $ sortOn ( spinID . snd)
                $ integratedAtPDOS
  let rIntgAll = unlines  [
                          rIntgAll'
                          , jdTable
                          ]
  putStrLn $ "===" ++ show intgTot
  T.writeFile texFile $ T.pack rIntgAll
    {-
  let rIntgAll' =
        rendertable
        $ (:) cleanedJdHead
        $ nub
        $ (:) (concat [ ["Total"]
                      , map (showDouble (2::Integer)) $ (\[a,b] -> [a,b,a-b,a+b]) $ intgTot
                      ])
        $ map (\(iu,idn,(_,(j,_))) -> j:map (showDouble (2::Integer)) [iu,idn,iu-idn,iu+idn] ) integratedAtPDOS
  putStrLn rIntgAll
    -------------------------------generating LaTex Representation------------------------
  resIntAll' <- markdownToTex rIntgAll
  let resIntAll = T.replace "\\}" "}"
                $ T.replace "\\{" "{" $ T.pack
                $ unlines [
                          "\\begin{longtable}[]{" ++ colAlign ++ "}"
                          , unlines $ tail $ lines $ T.unpack resIntAll'
                          ]

  T.writeFile texFile $ T.pack rIntgAll
  -}

  putStrLn "====!showTotPDOS===="

