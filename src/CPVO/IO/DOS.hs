{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module CPVO.IO.DOS (
    showTotPDOS
  ) where

import CPVO.Numeric
import CPVO.IO
import CPVO.IO.Reader.Ecalj.DOS

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List
import Numeric.LinearAlgebra

showTotPDOS :: [String] -> IO ()
showTotPDOS allArgs = do
--  (invStat, ymax, xmin, xmax, ctrlAtoms, uniqAtoms, ctrlAtomicAOs,jdTable, cleanedJdHead, foldernya, tailer, colAlign, texFile) <- readHeaderData allArgs
  Right (invStat, _ , _ , _ , _ , _ , ctrlAtomicAOs,jdTable, cleanedJdHead, foldernya, tailer, _ , texFile) <- readHeaderData allArgs

  -------------------------------generating DOS data------------------------
  totalDOS <- readTotalDOSText tailer foldernya
  -------------------------------integrating DOS data------------------------
  let intgTot = map (\i -> integrateToZero $ totalDOS ¿ [0,i]) $ flipBy invStat [1,2]
  pdosAtomic <- readPDOS invStat tailer foldernya ctrlAtomicAOs
  let integratedAtPDOS = integrateAtomicPDOS pdosAtomic
  putStrLn $ show integratedAtPDOS
  let rIntgAll' =
        rendertable
        $ (:) cleanedJdHead
        $ nub
        $ (:) (concat [ ["Total"]
                      , map (showDouble (2::Integer)) $ (\[a,b] -> [a,b,a-b,a+b]) $ intgTot
                      ])
        $ map (\(iu,idn,(_,(j,_))) -> j:map (showDouble (2::Integer)) [iu,idn,iu-idn,iu+idn] ) integratedAtPDOS
  let rIntgAll = unlines  [
                          rIntgAll'
                          , jdTable
                          ]
  putStrLn rIntgAll
  {-
    -------------------------------generating LaTex Representation------------------------
  resIntAll' <- markdownToTex rIntgAll
  let resIntAll = T.replace "\\}" "}"
                $ T.replace "\\{" "{" $ T.pack
                $ unlines [
                          "\\begin{longtable}[]{" ++ colAlign ++ "}"
                          , unlines $ tail $ lines $ T.unpack resIntAll'
                          ]
  -}

  T.writeFile texFile $ T.pack rIntgAll
  putStrLn "====done===="

