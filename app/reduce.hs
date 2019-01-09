#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import System.Environment
import Text.Printf (printf)

--import Control.Monad.IO.Class (MonadIO, liftIO)
--
import Control.Applicative
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Machine
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import System.IO (isEOF)
import Control.Monad (unless,replicateM_)

import Data.Machine.Mealy
import Data.Machine.Stack
import Data.List

main :: IO ()
main = do
--  res <- runT pipeline
  -- Remember that runT collects outputs in a list
--  putStrLn $ format "  " (printf "%.6f") $ fromRows res
  --res <- runT $ teeT zipping (source [1..]) lineSource
  (origin':_) <- getArgs
  let origin = read origin' :: Int
  res <- runT $ lineSource
              ~> skipHeader 2
              ~> echo
  let (lat',(atList:atNums':_:res')) = splitAt 3 res
  let atNums = map read $ words $ B8.unpack atNums' :: [Int]
  let (lat:coords:_) = map (tr' . fromRows . map getVec) [lat',res']

  let vOrigin = if (origin <= 0)  then 3 |> [0,0,0]
                                 else head $ toColumns $ coords ¿ [origin - 1]
  let newCoords = fromRows $ map (\a -> a - vOrigin) $ toColumns coords
  let filteredCoords = (<>) lat $
                        fromColumns $
                        filter cek1Quadrant $ filter (\x -> norm_2 x <= 1) $
        toColumns $ newCoords <> (inv lat)
  let atoms = concat $ Prelude.zipWith (replicate) atNums $ words $ B8.unpack atList
  let atomsNcoords = zip atoms $ toColumns coords
  let newAtoms = (\x -> zip (map head x) $ map length x) $
        group $ map fst $
        concatMap (\x -> filter (sameVec ("",x)) atomsNcoords) $
        toColumns filteredCoords

  putStrLn "Reduced Cell"
  putStrLn "1.0"
  putStr $ unlines $ tail $ lines $ dispf 6 $ tr' lat
  putStrLn $ unwords $ map fst newAtoms
  putStrLn $ unwords $ map (show . snd) newAtoms
  putStrLn "Cartesian"
--  putStr $ unlines $ tail $ lines $ dispf 6 $ tr' filteredCoords
  putStr $ unlines $ tail $ lines $ dispf 6 $ tr' $ (inv lat) <> filteredCoords
  putStr $ show coords
elemWith :: (a -> b -> Bool) -> a -> [b] -> Bool
elemWith fun needle hay =
  let cekF _ _ _ [] = False
      cekF res f n (h:hs) = if (f n h) then True
                                       else cekF res f n hs
   in cekF False fun needle hay


sameVec (_,v) (_,r) = (norm_2 $ v - r) < 0.0000001
cek1Quadrant' v = foldr (\a b -> (a >= 0) && b) True $ toList v

cek1Quadrant _ = True

cek1Quadrant'' v = let cekQ _ [] = True
                       cekQ r (a:as) = if  (a <= 0) then False
                                                 else cekQ r as
                    in cekQ True $ toList v


  {-
--genLat :: Monad m => ([Vector Double],ByteString)
--       -> ProcessT m (Int,ByteString) ([Vector Double],ByteString)

genLat :: Monad m => MachineT m (Is ([Vector Double], ByteString)) ([Vector Double], ByteString)
genLat = construct $ await >>= go
  where go cur@(l,v) |
          (length l < 3) = do
                            yield cur
                            next <- await
                            go $! func cur next
        func (a1,_) (i,b) = ((getVec b):a1,b)


genLat' :: Monad m => ([Vector Double],ByteString)
       -> ProcessT m (Int,ByteString) ([Vector Double],ByteString)
--genLat :: Monad m => ProcessT m a a
genLat' seed = construct $ go seed
  where
    go cur@(l,v)
      | (length l) < 3 = do
                            yield cur
                            next <- await
                            go $! func cur next
      | otherwise = yield (l,v)
    func (a1,_) (i,b) = ((getVec b):a1,b)

-}

genVec :: Monad m => ProcessT m BS.ByteString (Vector Double)
genVec = repeatedly $ do
  line <- await
  yield $ fromList $ map read $ words $ B8.unpack line

getVec :: ByteString -> Vector Double
getVec line =
  let xs = (|>) 3 $ map read $ words $ B8.unpack line
   in xs



skipHeader :: Monad m => Int -> ProcessT m a a
skipHeader n = dropping n

--lineNum :: Monad m => ProcessT m BS.ByteString (Int, BS.ByteString)

--lineSource :: MonadIO m => SourceT m ByteString
lineSource = ioSource isEOF BS.getLine

ioSource k f = construct go where
  go = do
    cond <- liftIO k
    unless cond $ do
      liftIO f >>= yield >> go

moveVec :: Monad m => Vector Double -> ProcessT m (Vector Double) (Vector Double)
moveVec v = repeatedly $ do
  x <- await
  yield $ x - v

