#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.List

import CPVO.Numeric
import CPVO.IO
import CPVO.IO.Type
import CPVO.IO.Reader.Util
import CPVO.IO.Reader.Ecalj.Util
import CPVO.IO.Reader.Ecalj.DOS

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T
import Data.Either (rights)
-------------------------
import Numeric.LinearAlgebra

--import CPVO.IO.Reader.Ecalj.MMOM (getMMOM)

import System.Environment (getArgs)

main :: IO ()
main = do
  allArgs <- getArgs
  putStrLn $ show allArgs
  {-
  let testArgs = [ "table03.tex"
                 ,"Atomic magnetic moment"
                 ,"{Atom Number}|{Atomic Orbital}|{Spin Density Integration}|{PDOS Integration}|{Difference}"
                  --colAlign like @{}llSSS@{}
                  , "@{}llSSS@{}"
                 ,"-9:6","25","T","o","1","nico2o4"
                 ,"extendedNiCo2O4.normal/nico2o4.0GGA"
                 ,"Ni:Ni#3d:6:7:9:8:10","Co:Co#3d:6:7:8:9:10","O:O#2p:3:4:5" ]
  -}
  getMMOM allArgs
  putStrLn "========beresbanh======="

readMMOM :: Int -> String -> IO [Double]
readMMOM nAtom foldernya = do
    fLLMF <- fmap (T.unpack . head) $ getLastLLMF foldernya
    putStrLn $ "===================processed LLMF=" ++ fLLMF
    mmom <- fmap (map T.double) $ inshell2text $ concat [ "mkdir -p temp; grep mmom ", fLLMF
                                            ,"| tail -n", show (nAtom + 1)
                                            ,"| head -n", show nAtom
                                            ,"| awk '{print $2}'"
                                          ]
    sdtMMOM <- fmap (map T.double) $ inshell2text $ concat [ "grep mmom ", fLLMF, "| grep ehf | tail -1 | sed -e 's/^.*mmom=//g'| awk '{print $1}'"
                                          ]
    return ( map fst $ rights $ concat [sdtMMOM,mmom])

----------------------------------------------------------------------
--getMMOM allArgs@(texFile:jd:jdHead:colAlign:xr:ymax':wTot:tumpuk:invS:tailer:foldernya:aos) = do

getMMOM :: [String] -> IO ()
getMMOM allArgs = do
--getMMOM allArgs = do
    putStrLn "===start ==== CPVO.IO.Reader.Ecalj.MMOM: getMMOM ==="
    Right (invStat,_,_,_,ctrlAtoms,_, ctrlAtomicAOs,jdTable, cleanedJdHead,foldernya,tailer,colAlign,texFile,_) <- readHeaderData allArgs

    -------------------------------generating data------------------------
    -------------------------------generating DOS data------------------------
    totalDOS <- readTotalDOSText tailer foldernya
    -------------------------------integrating DOS data-----------------------
    let intgTot = map (\i -> integrateToZero $ totalDOS ¿ [0,i]) $ flipBy invStat [1,2] -- run it on spin [1,2]
    putStrLn $ show intgTot
    -------------------------------integrating PDOS data------------------------
    let nAtom = length ctrlAtoms
    -------------------------------generating PDOS data------------------------
              -- map ditambah -1 karena input mengikuti gnuplot
              -- input : d kolom 6-10
              -- gnuplot : d kolom 6-10
              -- hmatrix : d kolom 5-9

  -------------------------------integrating PDOS data------------------------
    putStrLn $ "========invStat=" ++ (show invStat)
    (tMMomSD:mmomSD) <- fmap (map (* (invStat2Double invStat))) $ readMMOM nAtom foldernya

    pdosAtomicAll <- readPDOS invStat tailer foldernya [ Cetak s a | s <- flipBy invStat [1,2], a <- ctrlAtomicAOs ]
    let integratedAtomicPDOS = integrateAtomicPDOS pdosAtomicAll
--    let integratedAtomicPDOS = [(4.46416536665,Cetak {spinID = 2, atom = AO {atnum = 9, atsym = "Ni", labelAO = "Ni 3d", intAOs = Right [5,6,8,7,9]}}),(4.463953005124999,Cetak {spinID = 2, atom = AO {atnum = 10, atsym = "Ni", labelAO = "Ni 3d", intAOs = Right [5,6,8,7,9]}}),(1.8869112533249992,Cetak {spinID = 2, atom = AO {atnum = 13, atsym = "CoTd", labelAO = "Co_Td", intAOs = Right [5,6,7,8,9]}}),(1.8869449115749999,Cetak {spinID = 2, atom = AO {atnum = 14, atsym = "CoTd", labelAO = "Co_Td", intAOs = Right [5,6,7,8,9]}}),(3.3089247563250015,Cetak {spinID = 2, atom = AO {atnum = 11, atsym = "Co", labelAO = "Co 3d", intAOs = Right [5,6,7,8,9]}}),(3.3090371868,Cetak {spinID = 2, atom = AO {atnum = 12, atsym = "Co", labelAO = "Co 3d", intAOs = Right [5,6,7,8,9]}}),(1.640961028275002,Cetak {spinID = 2, atom = AO {atnum = 1, atsym = "O", labelAO = "O 2p", intAOs = Right [2,3,4]}}),(1.6409132271249995,Cetak {spinID = 2, atom = AO {atnum = 2, atsym = "O", labelAO = "O 2p", intAOs = Right [2,3,4]}}),(1.6320303127249973,Cetak {spinID = 2, atom = AO {atnum = 3, atsym = "O", labelAO = "O 2p", intAOs = Right [2,3,4]}}),(1.632206124225001,Cetak {spinID = 2, atom = AO {atnum = 4, atsym = "O", labelAO = "O 2p", intAOs = Right [2,3,4]}}),(1.6409606058749984,Cetak {spinID = 2, atom = AO {atnum = 5, atsym = "O", labelAO = "O 2p", intAOs = Right [2,3,4]}}),(1.6320295385999992,Cetak {spinID = 2, atom = AO {atnum = 6, atsym = "O", labelAO = "O 2p", intAOs = Right [2,3,4]}}),(1.632207104299999,Cetak {spinID = 2, atom = AO {atnum = 7, atsym = "O", labelAO = "O 2p", intAOs = Right [2,3,4]}}),(1.6409136864250018,Cetak {spinID = 2, atom = AO {atnum = 8, atsym = "O", labelAO = "O 2p", intAOs = Right [2,3,4]}}),(3.074838110349999,Cetak {spinID = 1, atom = AO {atnum = 9, atsym = "Ni", labelAO = "Ni 3d", intAOs = Right [5,6,8,7,9]}}),(3.0748409857000003,Cetak {spinID = 1, atom = AO {atnum = 10, atsym = "Ni", labelAO = "Ni 3d", intAOs = Right [5,6,8,7,9]}}),(4.658541561950002,Cetak {spinID = 1, atom = AO {atnum = 13, atsym = "CoTd", labelAO = "Co_Td", intAOs = Right [5,6,7,8,9]}}),(4.658537525849999,Cetak {spinID = 1, atom = AO {atnum = 14, atsym = "CoTd", labelAO = "Co_Td", intAOs = Right [5,6,7,8,9]}}),(3.3823911824999993,Cetak {spinID = 1, atom = AO {atnum = 11, atsym = "Co", labelAO = "Co 3d", intAOs = Right [5,6,7,8,9]}}),(3.382312940599999,Cetak {spinID = 1, atom = AO {atnum = 12, atsym = "Co", labelAO = "Co 3d", intAOs = Right [5,6,7,8,9]}}),(1.7342360182500012,Cetak {spinID = 1, atom = AO {atnum = 1, atsym = "O", labelAO = "O 2p", intAOs = Right [2,3,4]}}),(1.7342569759499995,Cetak {spinID = 1, atom = AO {atnum = 2, atsym = "O", labelAO = "O 2p", intAOs = Right [2,3,4]}}),(1.7405769262499997,Cetak {spinID = 1, atom = AO {atnum = 3, atsym = "O", labelAO = "O 2p", intAOs = Right [2,3,4]}}),(1.7405829991500015,Cetak {spinID = 1, atom = AO {atnum = 4, atsym = "O", labelAO = "O 2p", intAOs = Right [2,3,4]}}),(1.7342378289000024,Cetak {spinID = 1, atom = AO {atnum = 5, atsym = "O", labelAO = "O 2p", intAOs = Right [2,3,4]}}),(1.7405716288500022,Cetak {spinID = 1, atom = AO {atnum = 6, atsym = "O", labelAO = "O 2p", intAOs = Right [2,3,4]}}),(1.7405883332000014,Cetak {spinID = 1, atom = AO {atnum = 7, atsym = "O", labelAO = "O 2p", intAOs = Right [2,3,4]}}),(1.7342548785500007,Cetak {spinID = 1, atom = AO {atnum = 8, atsym = "O", labelAO = "O 2p", intAOs = Right [2,3,4]}})]

    putStrLn "==========show tMMomSD==========="
    putStrLn $ show tMMomSD
    putStrLn "==========show mmomSD==========="
    putStrLn $ show $ map (showDouble (3::Integer)) mmomSD
    putStrLn "==========show integratedAtomicPDOS==========="
    let diffIntegratedAtomicPDOS = sortOn (atnum . snd) $ map diffCetak $ groupBy (\a b -> atnumFromCetak a == atnumFromCetak b) $ sortOn atnumFromCetak $ integratedAtomicPDOS
    --putStrLn $ "====" ++ (show $ zip mmomSD $ map (atnum . snd) diffIntegratedAtomicPDOS)
    let rIntgAll' = id $ rendertable
      {-
         $ (:) cleanedJdHead
         $ (:) (concat [ ["Total" ]
           , ["  "]
           , map (showDouble (3::Integer)) $ (\[t,iu,idn] -> [t,iu-idn,t-(iu-idn)]) $ (tMMomSD:intgTot)
           ])
         $ zipWith (\a b -> a:b) (map show ([1,2..]::[Integer]))
         $ zipWith (\sdMom (intMom,(_,(j,_))) -> j:(map (showDouble (3::Integer)) [sdMom,intMom,sdMom-intMom])) mmomSD
         $ map (\(iu,idn,b) -> ((iu-idn),b) )
        -}
         $ (:) cleanedJdHead
         $ (:) (concat [ ["Total" ]
           , ["  "]
           , map (showDouble (3::Integer)) $ (\[t,iu,idn] -> [t,iu-idn,t-(iu-idn)]) $ (tMMomSD:intgTot)
           ])
         $ zipWith (\sdMom (intMom, a) -> (show $ atnum a):(labelAO a):(map (showDouble (3::Integer)) [sdMom,intMom,sdMom-intMom]) ) mmomSD
         $ diffIntegratedAtomicPDOS
    putStrLn $ rIntgAll'
    let rIntgAll = unlines  [
                            rIntgAll'
                            , jdTable
                            ]
                              {-
    resIntAll' <- markdownToTex rIntgAll
    let resIntAll = T.replace "\\}" "}"
                  $ T.replace "\\{" "{" $ T.pack
                  $ unlines [
                            "\\begin{longtable}[]{" ++ colAlign ++ "}"
                            , unlines $ tail $ lines $ T.unpack resIntAll'
                            ]
--    putStrLn rIntgAll
--    T.putStrLn resIntAll
--    -}
    T.writeFile texFile $ T.pack rIntgAll
    putStrLn "===done CPVO.IO.Reader.Ecalj.MMOM: getMMOM ==="
      where
        atnumFromCetak = atnum . atom . snd
        diffCetak ((i1, Cetak 1 a):(i2,Cetak 2 _):_) = (i1 - i2, a)
        diffCetak ((i2, Cetak 2 a):(i1,Cetak 1 _):_) = (i2 - i1, a)
        diffCetak _ = (0, ErrAtOrb)
