{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import qualified Language.C.Inline as C
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import qualified Data.Monoid as M ((<>))
import           Foreign.C.Types

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc

import Lib
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import System.Environment
import Text.Printf (printf)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Eigen.Matrix as M
import Data.List.Split

import Language.Fortran.Parser.Utils
import Data.Maybe
import Data.List
import Control.Monad

data GList = GList {
                    q2 :: Double,
                    qComponents :: Matrix Double
                   } deriving (Read, Show)

main :: IO ()
main = do
  (idJob:totalJob:header:[]) <- getArgs
  z <- readFile $ header ++ "glist"
  let (param:datanya) = lines z
      datagroup = groupBy (\a b-> head a == head b ) $ map (map $ fromJust . readReal ) $ map words $ datanya
      glist = filter (\a -> 10 < (sqrt $ q2 a))
                $ map (\(qin:coords) -> GList (last qin) (fromLists $ map (snd . (splitAt 2)) coords)) datagroup
  listMdx' <- readFile $ header ++ "mdxfile"
  let listMdx = map (header ++) $ drop 2 $ lines listMdx'
  a_lattice' <- readFile $ header ++ "md_str.dat"
  let a_lattice = read $ head $ head
          $ map words
          $ filter (isInfixOf "a_lattice")
          $ lines a_lattice' :: Double
-- perhitungannya sekarang,
-- qx --- <> rx ry  rz
-- qy ---    |  |   |
-- qz ---    |  |   |
-- mending q nya yang di transpose, jadi cuma transpose sekali, ga perlu transpose r berkali-kali
-- persiapan parallel

  let processList = floor $ (fromIntegral $ length glist)/(read totalJob :: Double)
{-
  putStrLn $ show idJob
  putStrLn $ show totalJob
  putStrLn $ show processList
  putStrLn $ show $ listMdx
  putStrLn $ show a_lattice
  -}
  let glistProcessed = if processList == length glist then glist
                                                      else chunksOf processList glist !! ((read idJob :: Int) - 1)  -- idJob= [1..] padahal index started from 0
--  let glistSample = head glistProcessed
  let glistSample = take 2 glistProcessed
--  let mdxF = head listMdx
  let !pengali = pi*pi/a_lattice -- ini sk.f:38 pai2/a_lattice

--  putStrLn $ show test
  putStrLn $ show "reading MDX ..."
  !mdxL <- mapM readMdx listMdx
  putStrLn $ show "reading MDX ... DONE"
  putStrLn $ show "running for GList ... "
--  test <- runForMdx pengali glistSample [] mdxL
  _ <- runForGlist pengali glistSample [] mdxL
  putStrLn $ show "DONE"

readMdx mdxF = do
  mdx' <- readFile mdxF
  let !mdx = tr $ fromLists $ map (map $ fromJust . readReal ) $ map words $ tail $ lines mdx' :: Matrix Double
  return mdx

runForGlist _ [] r _ = return r
runForGlist pengali (gl:gls) ts mdxL = do
  !t <- runForMdx pengali gl [] mdxL
  putStrLn $ show $ take 2 t
--  runForGlist pengali gls (t:ts) mdxL
  runForGlist pengali gls [] mdxL

runForMdx :: Double -> GList -> [(Double,Double)] -> [Matrix Double] -> IO [(Double,Double)]
runForMdx _ _ r [] = return r
runForMdx pengali glistS rs (mdxL:mdxLs) = do
  !x <- calcSingleG pengali glistS mdxL
  runForMdx pengali glistS (x:rs) mdxLs

calcSingleG :: Double -> GList -> Matrix Double -> IO (Double,Double)
calcSingleG pengali glistS mdxL = do
  let !dotRes = calcDot pengali mdxL glistS
  let cosSingle = sumElements $ cmap cos dotRes
  let sinSingle = sumElements $ cmap sin dotRes
  return $ (sinSingle,cosSingle)

calcDot pengali mdx gSample = scale pengali -- sk.f:38
    $ (qComponents gSample) <> mdx -- sk.f:36

{-
calcSingleMdx pengali mdx gSample = sumElements
    $ cmap cos -- sk.f:38
    $ scale pengali -- sk.f:38
    $ (qComponents gSample) <> mdx -- sk.f:36
    -}

