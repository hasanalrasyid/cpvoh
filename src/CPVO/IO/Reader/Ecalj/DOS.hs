{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module CPVO.IO.Reader.Ecalj.DOS (
  readPDOS
  )
  where

import CPVO.Numeric
import CPVO.IO
import CPVO.IO.Type
import Text.Printf as TP
import Numeric.LinearAlgebra
import qualified System.Process as SP
import System.IO (openTempFile,hClose)
-- ===============================================

readPDOS :: InvStat -> String -> Directory -> [Cetak]
         -> IO [(Matrix Double, Cetak)]
readPDOS invStat tailer dir ctrlAtAOs = do
  putStrLn "========readPDOS"
  putStrLn $ "====ctrlAtAOs " ++ show ctrlAtAOs
  putStrLn $ "===invStat " ++ show invStat
  putStrLn $ "===tailer" ++ show tailer
  sequence [ readOnePDOS dir tailer j | j <- ctrlAtAOs ]

readOnePDOS :: String
            -> String -> Cetak
            -> IO (Matrix Double, Cetak )
readOnePDOS theFolder tailing (Cetak spin atTarget@(AO noAt _ labelAt iAOs)) = do
  putStrLn "=========readOnePDOS@src/CPVO/IO/Reader/Ecalj/DOS.hs"
  let namaFao = theFolder ++ "/dos.isp" ++ show spin ++ ".site" ++ (TP.printf "%03d" noAt) ++ "." ++ tailing
  (tmpfile,h) <- openTempFile "temp" "aEDOS.suffix"
  hClose h
  _ <- SP.system $ "mkdir -p temp; sed '{1d}' " ++ namaFao ++ " > " ++ tmpfile
  aoE <- fmap (\x -> (¿) x [0]) $ loadMatrix $ tmpfile -- 0th column, Energy column
  let zeroE = asColumn $ konst 0 (rows aoE)                                      -- zero valued column instead of real column
  aPDOS <- fmap (dropColumns 1) $ getPDOS' zeroE tmpfile iAOs [namaFao]                                -- create sum of per atomic AOs (PDOS/atom)
  putStrLn "=========readOnePDOS@src/CPVO/IO/Reader/Ecalj/DOS.hs"
  return $ (fromBlocks [[aoE, aPDOS]] , (Cetak spin (atTarget { labelAO = hashSpaceText labelAt} )))
readOnePDOS _ _ _ = return (matrix 1 [1..], NoCetak)

getPDOS' :: Matrix Double -> String -> [Int] -> [String] -> IO (Matrix Double)
getPDOS' res _ _ []  = return res
getPDOS' res tmpf iAOs (nf:nfiles)  = do
  _ <- SP.system $ "mkdir -p temp; more +2 " ++ nf ++ " > " ++ tmpf
  aPDOS' <- fmap (\x -> sumRow $ (¿) x iAOs) $ loadMatrix tmpf
  getPDOS' (fromBlocks [[res,asColumn aPDOS']]) tmpf iAOs nfiles
