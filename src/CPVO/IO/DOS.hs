{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module CPVO.IO.DOS (
    showTotPDOS
  ) where

import CPVO.Numeric
import CPVO.IO
import CPVO.IO.Reader.Ecalj.Common
import CPVO.IO.Reader.Ecalj.DOS

import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List.Split
import Data.List
import Data.Maybe
import Numeric.LinearAlgebra

showTotPDOS :: [String] -> IO ()
showTotPDOS allArgs = do
  (invStat, ymax, xmin, xmax, ctrlAtoms, uniqAtoms, ctrlAtomicAOs,jdTable, cleanedJdHead, foldernya, tailer, colAlign, texFile) <- readHeaderData allArgs

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
                      , map (showDouble 2) $ (\[a,b] -> [a,b,a-b,a+b]) $ intgTot
                      ])
        $ map (\(iu,id,(_,(j,_))) -> j:map (showDouble 2) [iu,id,iu-id,iu+id] ) integratedAtPDOS
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

