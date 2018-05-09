{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module CPVO.IO (
    showDouble
    , markdownToTex
    , shell2list
    , table
    , pdosA'
    , getPDOS
  ) where

import CPVO.Numeric

import Text.Printf as TP
import Text.Pandoc
import qualified Data.Text as T
import Control.Monad ((<=<))

import Turtle hiding (sortBy,char,text)
--x--
--x-- --import Turtle.Helper
import qualified Control.Foldl as Fold
--x-- import System.Environment (getArgs)
--x-- import qualified Data.Text as T
--x-- import qualified Data.Text.IO as T
--x-- import qualified Data.Text.Read as T
--x-- import qualified Data.Text.Format as T
--x-- import Text.Printf as TP
--x-- import Data.List.Split
import Data.List
--x-- import Data.Maybe
--x-- import Data.Either
--x-- -------------------------
--x--
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Devel (readMatrix)
import Numeric.LinearAlgebra.Data hiding (find)
--x-- import Data.Char
import System.Process as SP
import System.IO (openTempFile,hClose)
--x-- -------------------------
import Text.PrettyPrint.Boxes hiding ((<>),cols,rows)
import qualified Text.PrettyPrint.Boxes as TB
--x-- import Data.List
-- ===============================================
-- start of Accelerate
-- import Data.Array.Accelerate              as A
-- import Data.Array.Accelerate.LLVM.Native  as CPU
-- import Data.Array.Accelerate.LLVM.PTX     as GPU
--
{-
dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)

main1 = do
    kj@(texFile:jd:xr:ymax':wTot:tumpuk:invS:tailer:foldernya:aos) <- getArgs
    putStrLn $ show kj
--    let xs = fromList (Z:.10) [0..]   :: Vector Float
--    let ys = fromList (Z:.10) [1,3..] :: Vector Float
--    CPU.run $ dotp (use xs) (use ys)
-}

showDouble _ 0 = show 0
showDouble i x = (flip TP.printf) x $ concat ["%0.",show i,"f"]


-- Prosesor untuk pandoc
-- LaTeX writer...
--
markdownToTex str = runIOorExplode
              $ ((writeLaTeX def) <=< (readMarkdown def{
                  readerExtensions = foldr enableExtension pandocExtensions [Ext_tex_math_dollars, Ext_raw_tex, Ext_table_captions]
                                                       }))
              $ T.pack $ str
--------------------------------------------------------------------------------------
shell2list :: MonadIO io =>  Shell a->  io [a]
shell2list xx = fold (xx) Fold.list

--------------------------------------------------------------------------------------
-- Text Formatting
--
pad width x = x ++ replicate k ' '
  where k = width - length x

fmt_column :: [String] -> Box
fmt_column items = vcat left (addHead $ map (text.pad width) items)
  where width = maximum $ map length items
        hsep = text ( replicate width '-' )
        addHead (a:as) = a:hsep:as

table :: [[String]] -> Box
table rs = vsep TB.<> hcat top (intersperse vsep (map fmt_column columns)) TB.<> vsep
  where
    columns = transpose rs
    nrows = length rs
    vsep =  vcat left $ map char ("|" ++ (concat $ replicate nrows "|"))

caller :: T.Text
caller = T.unlines  [ "callme with : genPDOSAtomicOrbital.hs [Atom:Orbital]                tailer    folder"
                    , "ex.         : genPDOSAtomicOrbital.hs 'O NiTd:2:3:4:5 CoTd:2:3:4:5' nico2o4   nico2o4.invB.0GGA"
                    ]

hashSpaceText t = T.unpack $ T.replace "#" " " $ T.pack t

---------------------------------------------------------------------------
getAllPDOS (s,n,pd) = do
  let pdTot = getY0 pd
  return (s,n,pdTot)

takeAO i [] = []
takeAO i (a@(n,_,_):as) = if (i == n) then [a]
                                     else takeAO i as

pdosA' :: String -> String -> [((String, String, [Int]), [(Int, String)]) -> IO (Int,String, Matrix Double)]
pdosA' foldernya tailer = fmap (getPDOS foldernya tailer) [1,2]
---------------------------------------------------
getPDOS' res _ _ []  = return res
getPDOS' res tmpf intAOs (nf:nfiles)  = do
  _ <- SP.system $ "mkdir -p temp; more +2 " ++ nf ++ " > " ++ tmpf
--  aoDOS' <- fmap (\x -> (¿) x intAOs) $ loadMatrix tmpf
  aPDOS' <- fmap (\x -> sumRow $ (¿) x intAOs) $ loadMatrix tmpf
  getPDOS' (fromBlocks [[res,asColumn aPDOS']]) tmpf intAOs nfiles

-------------------------------------------------------------
-- Input Processing: read PDOS data
--
getPDOS :: String -> String -> Int -> ((String, String, [Int]), [(Int, String)]) -> IO (Int,String, Matrix Double)
getPDOS theFolder tailing spin (a@(namaAtom,jdAtom,intAOs),lsAtoms) = do
  let namaFaos = map (\(x,_) -> theFolder ++ "/dos.isp" ++ show spin ++ ".site" ++ (TP.printf "%03d" x) ++ "." ++ tailing) lsAtoms
  (tmpfile,h) <- openTempFile "temp" "aEDOS.suffix"
  hClose h
  _ <- SP.system $ "mkdir -p temp; more +2 " ++ (head namaFaos) ++ " > " ++ tmpfile -- this is needed only to generate aoE, the real processing is in getPDOS'
  aoE <- fmap (\x -> (¿) x [0]) $ loadMatrix tmpfile                             -- 0th column, Energy column
  let zeroE = asColumn $ konst 0 (rows aoE)                                      -- zero valued column instead of real column
  aPDOS <- fmap (dropColumns 1) $ getPDOS' zeroE tmpfile intAOs namaFaos                                -- create sum of per atomic AOs (PDOS/atom)
--  putStrLn $ show $ sumRow aPDOS
--  let pDOS = sumRow aPDOS -- create sum of aPDOS (atomic PDOS/cell)
--  return $ (spin, hashSpaceText jdAtom, fromBlocks [[aoE, asColumn pDOS]])
  return $ (spin, hashSpaceText jdAtom, fromBlocks [[aoE, aPDOS]])
------------------------------------------------------------------
