#!/usr/bin/env stack
--stack --resolver lts-14.27 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

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

main :: IO ()
main = do
  arah' <- getArgs
  let arah = 3 |>  (map read arah' :: [Double])
  res <- runT (pipeline arah)
  -- Remember that runT collects outputs in a list
  putStrLn $ format "  " (printf "%.6f") $ fromRows res
  where pipeline t = lineSource ~> genVec ~> moveVec t

lineSource :: MonadIO m => SourceT m ByteString
lineSource = ioSource isEOF BS.getLine

ioSource :: MonadIO m => IO Bool -> IO a -> SourceT m a
ioSource k f = construct go where
  go = do
    cond <- liftIO k
    unless cond $ do
      liftIO f >>= yield >> go

genVec :: Monad m => ProcessT m BS.ByteString (Vector Double)
genVec = repeatedly $ do
  line <- await
  yield $ fromList $ map read $ words $ B8.unpack line

moveVec :: Monad m => Vector Double -> ProcessT m (Vector Double) (Vector Double)
moveVec v = repeatedly $ do
  x <- await
  yield $ x - v

