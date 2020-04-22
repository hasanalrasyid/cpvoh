#!/usr/bin/env stack
--stack --resolver lts-14.27 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Numeric.LinearAlgebra
import Control.Monad.IO.Class (liftIO)
import Data.Machine hiding (zipWith)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import System.IO (isEOF)
import Control.Monad (unless)
import Data.List
import System.Environment (getArgs)
import Prelude hiding ((<>))

main :: IO ()
main = do
  help <- getArgs
  if (null help) then runJob
                 else runHelp

runHelp :: IO ()
runHelp = putStr $ unlines $
  "Primitive Lattice Generator" :
  "run with:":
  "reduce < input.vasp":
  "input: VASP file format":
  "Title":
  "1.0":
  "row based lattice vectors that will be used in global basis":
  "Atom List":
  "Number of Atom List":
  "Cartesian":
  "complete list of Cartesian coordinates using global basis":
  "EXAMPLE INPUT":
  "1.0":
  "-1.892500   1.892500   4.759800":
  " 1.892500  -1.892500   4.759800":
  " 1.892500   1.892500  -4.759800":
  "Ti O":
  "2 4":
  "Cartesian":
  "0.000000  0.000000   0.000000":
  "0.000000  1.892500   2.379900":
  "1.892500  1.892500  -2.759065":
  "0.000000  1.892500   4.380634":
  "0.000000  1.892500   0.379166":
  "1.892500  1.892500   2.759066":
  []

runJob :: IO ()
runJob = do
  let origin = 0
  putStrLn "Reduced Cell all in Angstrom, help with reduce.hs -h"
  putStrLn "1.0"
  res <- runT $ lineSource
              ~> skipHeader 2
              ~> echo
  let (lat',(atList:atNums':_:res')) = splitAt 3 res
  let atNums = map read $ words $ B8.unpack atNums' :: [Int]
  let (lat:coords:_) = map (fromColumns . map getVec) [lat',res']
  let vOrigin = if (origin <= 0)  then zeroVec
                                 else head $ toColumns $ coords ¿ [origin - 1]
  let atoms = concat $ zipWith (replicate) atNums $ words $ B8.unpack atList
  let atomsNcoords = zip atoms $ toColumns coords

  let movedCoords = fromColumns $ map (\a -> a - vOrigin) $ toColumns coords
  let primLat_O = lat
  let primCoords = fromColumns $
                   removeMirrorEdge $
                   filter (not . isCorner) $
                   filter cekFirstQuadrant $
                   toColumns $ (inv primLat_O) <> movedCoords
  let primAtoms =  concatMap (\x -> filter (sameVec ("",x)) atomsNcoords) $
        toColumns $ lat <> primCoords
  putStr $ onlyMatrix $ tr' lat
  putStrLn $ unwords $ map head $ group $ map fst primAtoms
  putStrLn $ unwords $ map (show . length) $ group $ map fst primAtoms
  putStrLn "Cartesian"
  putStr $ onlyMatrix $ tr' $ lat <> primCoords
  putStrLn "====endOf VASP, cut here======"
  putStrLn "Cartesian in Bohr"
  putStr $ onlyMatrix $ tr' $ cmap toBohr $ lat <> primCoords

toBohr angs = angs * 1.88973

onlyMatrix m = unlines $ tail $ lines $ dispf 6 m
sameVec (_,v) (_,r) = sV v r
sV v r = (norm_2 $ v - r) < 0.0000001
t1 a b = (1 - norm_2 (a - b) )^2 < 0.000001
cekFirstQuadrant v = foldr (\a b -> (0 <= a) && (a <= 1) && b) True $ toList v

isCorner i
  | (i == zeroVec) = False
  | otherwise = elem True $ map (t1 i) $ p111 ++ p011 ++ p001

p011 = map fromList $ nub $ permutations $ toList $ 3 |> [0,1,1]
p001 = map fromList $ nub $ permutations $ toList $ 3 |> [0,0,1]
p111 = [ 3 |> [1,1,1] ]
zeroVec :: Vector Double
zeroVec = 3 |> [0,0,0]
removeMirrorEdge i =
  let a = [(x,y) | x <- i, y <- i]
      d (v,w,_) = sort [v,w]
      e (a,b) = (a,b,t1 a b)
      c = map head $ nub $ map d $ filter (\(_,_,c) -> c) $ map e a
   in i \\ c

getVec :: B8.ByteString -> Vector Double
getVec line =
  let xs = (|>) 3 $ map read $ words $ B8.unpack line
   in xs
skipHeader :: Monad m => Int -> ProcessT m a a
skipHeader n = dropping n
lineSource = ioSource isEOF BS.getLine

ioSource k f = construct go where
  go = do
    cond <- liftIO k
    unless cond $ do
      liftIO f >>= yield >> go
