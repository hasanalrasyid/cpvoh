{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}


module Test where

import Foreign
import Foreign.C
--import Foreign.Ptr
--
-- import CPVO.Numeric
-- import CPVO.IO
-- import CPVO.IO.Reader.Ecalj.Common
--
--
--
--
-- import qualified Control.Foldl as Fold
-- import System.Environment (getArgs)
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as T
-- import qualified Data.Text.Read as T
-- import qualified Data.Text.Format as T
-- import Text.Printf
-- import Data.List.Split
-- import Data.List
-- import Data.Maybe
-- import Data.Either
-- -------------------------
--
-- import Numeric.LinearAlgebra
-- import Numeric.LinearAlgebra.Devel (readMatrix)
-- import Numeric.LinearAlgebra.Data hiding (find)
--import Data.Char
--import System.Process
--import System.IO
-------------------------
--import Text.PrettyPrint.Boxes hiding ((<>),cols,rows)
--import qualified Text.PrettyPrint.Boxes as TB
--import Data.List
-------------------------
--import Text.Pandoc
--import Control.Monad ((<=<),forM)


foreign export ccall sumRootsInH :: Ptr Int -> Ptr CDouble -> Ptr CDouble -> IO ()
foreign export ccall hiHask :: IO ()

hiHask :: IO ()
hiHask = putStrLn "=====HASK======"

sumRootsInH :: Ptr Int -> Ptr CDouble -> Ptr CDouble -> IO ()
sumRootsInH n' xs' result = do
  n <- peek n'
  putStrLn $ "============" ++ (show n)
  xs <- peekArray n xs'
  poke result $ sumRoots xs
  return ()

sumRoots :: [CDouble] -> CDouble
sumRoots xs = sum (map sqrt xs)

testArgs :: [String]
testArgs = concat [ [ "GWtable01.tex"
                   , "Total and partial DOS at *E*~F~ in states/eV/unit-cell, QSGW method."
                   , "|*N*$_\\uparrow$(*E*~F~)|*N*$_\\downarrow$(*E*~F~)|*N*(*E*~F~)"
                     ]
                   , words "@{}lSSS@{} -9:6 25 T o flipSpin nico2o4 extendedNiCo2O4.normal/nico2o4.6G10 Ni:Ni#3d:6:7:9:8:10 Co:Co#3d:6:7:8:9:10 O:O#2p:3:4:5"
                   ]


