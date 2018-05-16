{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

--x-- import Turtle hiding (sortBy,char,text)

import CPVO.IO
import CPVO.Numeric
import CPVO.IO.DOS
import CPVO.IO.Reader.Ecalj.Common
import CPVO.IO.Reader.Ecalj.MMOM
import CPVO.IO.Plot.Gnuplot.DOS
import CPVO.IO.Plot.Gnuplot.Common
import CPVO.IO.Plot.PDOS


-------
import Data.Text as T
import Turtle
import Data.Either

--x--
--x-- --import Turtle.Helper
--x-- import qualified Control.Foldl as Fold
import System.Environment (getArgs)
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as T
-- import qualified Data.Text.Read as T
-- --x-- --import qualified Data.Text.Format as T
-- --x-- import Text.Printf as TP
-- import Data.List.Split
-- import Data.List
-- import Data.Maybe
-- import Data.Either (rights)
-- --x-- -------------------------
-- import Numeric.LinearAlgebra
-- --x-- import Numeric.LinearAlgebra.Devel (readMatrix)
-- --x-- import Numeric.LinearAlgebra.Data hiding (find)
-- import Data.Char (ord)
-- --x-- import System.Process as SP
-- --x-- import System.IO
-- --x-- -------------------------
-- --x-- import Text.PrettyPrint.Boxes hiding ((<>),cols,rows)
-- --x-- import qualified Text.PrettyPrint.Boxes as TB
-- --x-- import Data.List
-- --x-- -- ===============================================
-- -- start of Accelerate
-- -- import Data.Array.Accelerate              as A
-- -- import Data.Array.Accelerate.LLVM.Native  as CPU
-- -- import Data.Array.Accelerate.LLVM.PTX     as GPU
-- --

main = do
  putStrLn "========start test/f1.hs======="
  let testArgs = [ "table.tex"
                 ,"Atomic magnetic moment"
                 ,"{Number}|{Orbital}|{Spin Density}|{PDOS}|{Diff}"
                 -- column alignment options, sumthing like
                 -- @{}lSSS@{} in \begin{longtable}[]{@{}lSSS@{}}
                 , "@{}llSSS@{}"
                 ,"-9:6","25","T","o","flipSpin","nico2o4"
                 ,"extendedNiCo2O4.normal/nico2o4.6G10"
                 ,"Ni:Ni#3d:6:7:9:8:10","Co:Co#3d:6:7:8:9:10","O:O#2p:3:4:5" ]
  let testArgs1 = [ "fig1"
                 , "-9:3"
                 , "25" , "o" ,"flipSpin/keepSpin", "top:right", "T"
                 , "extendedNiCo2O4.normal/nico2o4.0GGA"
                 , "Ni:Ni#3d:6:7:9:8:10", "Co:Co#3d:6:7:8:9:10", "O:O#2p:3:4:5"
                 ]
  let testArgs2 = [ "QSGW_{0}"
                 , "-9:2", "55", "T"
                 , "o", "1"
                 , "nico2o4"
                 , "extendedNiCo2O4.normal/nico2o4.0GGA"
                 ,"Ni:Ni#3d:6:7:9:8:10", "Co:Co#3d:6:7:8:9:10", "O:O#2p:3:4:5"
                 ]

-- plotPDOS testArgs1
--  readHeaderData testArgs
  showTotPDOS testArgs
  putStrLn "========beres======="

