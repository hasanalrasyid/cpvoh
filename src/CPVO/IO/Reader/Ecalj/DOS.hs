{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module CPVO.IO.Reader.Ecalj.DOS (
  readPDOS
  )
  where

import CPVO.Numeric
import CPVO.IO
--
import Text.Printf as TP
import Numeric.LinearAlgebra
import qualified System.Process as SP
import System.IO (openTempFile,hClose)
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- ===============================================

getPDOS' res _ _ []  = return res
getPDOS' res tmpf intAOs (nf:nfiles)  = do
  _ <- SP.system $ "mkdir -p temp; more +2 " ++ nf ++ " > " ++ tmpf
  aPDOS' <- fmap (\x -> sumRow $ (¿) x intAOs) $ loadMatrix tmpf
  getPDOS' (fromBlocks [[res,asColumn aPDOS']]) tmpf intAOs nfiles

-------------------------------------------------------------
-- Input Processing: read PDOS data
--
readPDOS :: Double -> String -> String -> [(Int, ([Char], (String, [Int])))]
         -> IO [(Matrix Double, (Int, (String, (Int, String))))]
readPDOS invStat tailer dir ctrlAtAOs = do
  putStrLn "========readPDOS"
  putStrLn $ show ctrlAtAOs
  sequence
    $ (\x ->  [f a | f <- (readPDOS' invStat dir tailer), a <- x]) ctrlAtAOs

readPDOS' invStat foldernya tailer = fmap (readOnePDOS foldernya tailer) $ flipBy invStat [1,2] -- spin 1 up n spin 2 down

--readPDOS :: String -> String -> Int -> ((String, String, [Int]), [(Int, String)]) -> IO (Int,String, Matrix Double)
--readPDOS ::(spin, (noAt, (symAt, (labelAt, PDOS :: Matrix Double))))
readOnePDOS theFolder tailing spin inp@(noAt,(symAt,(labelAt,intAOs))) = do
  let namaFao = theFolder ++ "/dos.isp" ++ show spin ++ ".site" ++ (TP.printf "%03d" noAt) ++ "." ++ tailing
  (tmpfile,h) <- openTempFile "temp" "aEDOS.suffix"
  hClose h
  _ <- SP.system $ "mkdir -p temp; sed '{1d}' " ++ namaFao ++ " > " ++ tmpfile
  aoE <- fmap (\x -> (¿) x [0]) $ loadMatrix $ tmpfile -- 0th column, Energy column
  let zeroE = asColumn $ konst 0 (rows aoE)                                      -- zero valued column instead of real column
  aPDOS <- fmap (dropColumns 1) $ getPDOS' zeroE tmpfile intAOs [namaFao]                                -- create sum of per atomic AOs (PDOS/atom)
  return $ (fromBlocks [[aoE, aPDOS]] , (spin, (hashSpaceText labelAt, (noAt, symAt))))
------------------------------------------------------------------

-- getPDOS :: String -> String -> Int -> ((String, String, [Int]), [(Int, String)]) -> IO (Int,String, Matrix Double)
-- getPDOS theFolder tailing spin (a@(namaAtom,jdAtom,intAOs),lsAtoms) = do
--   let namaFaos = map (\(x,_) -> theFolder ++ "/dos.isp" ++ show spin ++ ".site" ++ (TP.printf "%03d" x) ++ "." ++ tailing) lsAtoms
--   (tmpfile,h) <- openTempFile "temp" "aEDOS.suffix"
--   hClose h
--   _ <- inshell2text $ "mkdir -p temp; more +2 " ++ (head namaFaos) ++ " > " ++ tmpfile -- this is needed only to generate aoE, the real processing is in getPDOS'
--   aoE <- fmap (\x -> (¿) x [0]) $ loadMatrix tmpfile                             -- 0th column, Energy column
--   let zeroE = asColumn $ konst 0 (rows aoE)                                      -- zero valued column instead of real column
--   aPDOS <- fmap (dropColumns 1) $ getPDOS' zeroE tmpfile intAOs namaFaos                                -- create sum of per atomic AOs (PDOS/atom)
--   return $ (spin, hashSpaceText jdAtom, fromBlocks [[aoE, aPDOS]])
-- ------------------------------------------------------------------
--
