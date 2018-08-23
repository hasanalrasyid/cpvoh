{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module CPVO.IO.Reader.Ecalj.MMOM where
import CPVO.Numeric
import CPVO.IO
import CPVO.IO.Reader.Ecalj.Common
import CPVO.IO.Reader.Ecalj.DOS

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Either (rights)
-------------------------
import Numeric.LinearAlgebra
import Data.Char (ord)

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
getMMOM allArgs@(texFile:jd:jdHead:colAlign:xr:ymax':wTot:tumpuk:invS:tailer:foldernya:aos) = do
--getMMOM allArgs = do
    putStrLn "===start ==== CPVO.IO.Reader.Ecalj.MMOM: getMMOM ==="
    (invStat, ymax, xmin, xmax, ctrlAtoms, uniqAtoms, ctrlAtomicAOs,jdTable, cleanedJdHead, foldernya, tailer,colAlign,texFile) <- readHeaderData allArgs

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
    (tMMomSD:mmomSD) <- fmap (map (* invStat)) $ readMMOM nAtom foldernya
    putStrLn $ show $ map (showDouble 3) mmomSD
    putStrLn $ show tMMomSD
    putStrLn "==========show tMMomSD==========="
    pdosAtomicAll <- readPDOS invStat tailer foldernya ctrlAtomicAOs
    let integratedAtomicPDOS = integrateAtomicPDOS pdosAtomicAll
    let rIntgAll' = rendertable
         $ (:) cleanedJdHead
         $ (:) (concat [ ["Total" ]
           , ["  "]
           , map (showDouble 3) $ (\[t,iu,id] -> [t,iu-id,t-(iu-id)]) $ (tMMomSD:intgTot)
           ])
         $ zipWith (\a b -> a:b) (map show [1,2..])
         $ zipWith (\sdMom (intMom,(_,(j,_))) -> j:(map (showDouble 3) [sdMom,intMom,sdMom-intMom])) mmomSD
         $ map (\(iu,id,b) -> ((iu-id),b) ) integratedAtomicPDOS
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
    putStrLn rIntgAll
    T.putStrLn resIntAll
    -}
    T.writeFile texFile $ T.pack rIntgAll
    putStrLn "===done CPVO.IO.Reader.Ecalj.MMOM: getMMOM ==="
