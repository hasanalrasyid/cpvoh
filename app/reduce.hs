#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

{-# LANGUAGE RankNTypes #-}

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
import Control.Monad (unless)

import Data.Machine.Mealy

main :: IO ()
main = do
--  res <- runT pipeline
  -- Remember that runT collects outputs in a list
--  putStrLn $ format "  " (printf "%.6f") $ fromRows res
  res <- runT $ teeT zipping (source [1..]) lineSource
              ~> skipHeader 2
              ~> echo
  putStrLn $ show res
  --where pipeline = lineSource ~> skipHeader ~> genVec ~> genLat ~> echo



--genLat :: Monad m => ProcessT m (Vector Double) ([Vector Double],Vector Double)
genLat :: Monad m => ProcessT m a a
genLat = dropping 3

genVec :: Monad m => ProcessT m BS.ByteString (Vector Double)
genVec = repeatedly $ do
  line <- await
  yield $ fromList $ map read $ words $ B8.unpack line

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

