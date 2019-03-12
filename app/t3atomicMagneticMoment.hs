#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

import CPVO.Numeric
import CPVO.IO
import CPVO.IO.Type
import CPVO.IO.Reader.Util
import CPVO.IO.Reader.Ecalj.Util
import CPVO.IO.Reader.Ecalj.DOS

import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Either (rights)
import Data.List
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
    Right (invStat,_,_,_,ctrlAtoms,_, ctrlAtomicAOs,jdTable, cleanedJdHead,foldernya,tailer,_,texFile,_) <- readHeaderData allArgs

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
    putStrLn $ show $ map (showDouble (3::Integer)) mmomSD
    putStrLn $ show tMMomSD
    putStrLn "==========show tMMomSD==========="
    pdosAtomicAll <- readPDOS invStat tailer foldernya
      [ Cetak s a | s <- flipBy invStat [1,2], a <- ctrlAtomicAOs ]
    let integratedAtomicPDOS = integrateAtomicPDOS pdosAtomicAll
    let rIntgAll' = id -- rendertable
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
--         $ map go5
         $ groupBy go6
         $ map go4
         $ groupBy (\(_,a) (_,b) -> atom a == atom b)
         $ sortBy (\x y -> compare (atom $ snd x) (atom $ snd y))
         $ integratedAtomicPDOS
    debugIt "rIntgAll'=====" rIntgAll'
    {-
    let rIntgAll = unlines  [
                            rIntgAll'
                            , jdTable
                            ]
    resIntAll' <- markdownToTex rIntgAll
    let resIntAll = T.replace "\\}" "}"
                  $ T.replace "\\{" "{" $ T.pack
                  $ unlines [
                            "\\begin{longtable}[]{" ++ colAlign ++ "}"
                            , unlines $ tail $ lines $ T.unpack resIntAll'
                            ]
    putStrLn rIntgAll
    T.putStrLn resIntAll
    T.writeFile texFile $ T.pack rIntgAll
    -}
    putStrLn "===done CPVO.IO.Reader.Ecalj.MMOM: getMMOM ==="
      where
        go5 a = let aos = map fst a
                    vs = map snd a
                    vals = toList $ sumCol $ fromLists vs
                 in vals
                 -- in zip aos $ zipWith (\c b -> c ++ [b]) vs vals
        go4 a@((_,b):_) = (atom b,map fst a)
        go4 _ = (ErrAtOrb,[])
        go6 (a,_) (b,_) = sameLabelOfAtoms a b
