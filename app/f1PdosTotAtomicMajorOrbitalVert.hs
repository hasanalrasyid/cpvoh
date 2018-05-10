{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

--x-- import Turtle hiding (sortBy,char,text)
import Lib (someFunc)
import CPVO.Numeric (integrateAll,testFunc,getY0,delta)
import CPVO.IO
import CPVO.IO.MMOM
import CPVO.IO.Plot.PDOS

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
  allArgs <- getArgs
  {-
  let testArgs1 = [ "fig2"
                 , "-9:2"
                 , "55" , "o" ,"1", "top:right", "T"
                 , "extendedNiCo2O4.normal/nico2o4.1G0"
                 , "Ni:Ni#3d:6:7:9:8:10", "Co:Co#3d:6:7:8:9:10", "O:O#2p:3:4:5"
                 ]
  let testArgs = [ "QSGW_{0}"
                 , "-9:2", "55", "T"
                 , "o", "1"
                 , "nico2o4"
                 , "extendedNiCo2O4.normal/nico2o4.0GGA"
                 ,"Ni:Ni#3d:6:7:9:8:10", "Co:Co#3d:6:7:8:9:10", "O:O#2p:3:4:5"
                 ]
  -}
  plotPDOS allArgs
  putStrLn "========beres======="
