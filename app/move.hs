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
import Data.ByteString (ByteString)
import System.IO (isEOF)
import Data.Char (ord)
import Control.Monad (unless)

main :: IO ()
main = do
  n <- runT pipeline
  -- Remember that runT collects outputs in a list
  print (head n)
  where pipeline = lineSource ~> countCommas ~>
                   filterBad ~> count

count :: Monad m => ProcessT m a Int
count = construct (go 0) where
  go n = do
    x <- await <|> (yield n *> stop)
    go (n + 1)

lineSource :: MonadIO m => SourceT m ByteString
lineSource = ioSource isEOF BS.getLine

countCommas :: Monad m => ProcessT m BS.ByteString Int
countCommas = repeatedly $ do
  line <- await
  yield (BS.count comma line)
  where comma = fromIntegral (ord ',')

filterBad :: Monad m => ProcessT m Int Int
filterBad = filtered (/=2)

ioSource :: MonadIO m => IO Bool -> IO a -> SourceT m a
ioSource k f = construct go where
  go = do
    cond <- liftIO k
    unless cond $ do
      liftIO f >>= yield >> go

main1 :: IO ()
main1 = do
  arah' <- getArgs
  st <- getLine
  let arah = 3 |>  (map read arah' :: [Double])
  let vCoord1 = 3 |> (map read $ words st :: [Double])
  let vX = vCoord1 - arah

--  putStrLn $ format "  " (printf "%.6f") vCF1s
--  putStrLn "============================="
  putStrLn $ format "  " (printf "%.6f") $ asRow vX
    where
      f a x = x - a


{-
main :: IO ()
main = do
  (f1:st) <- getArgs
  vCF1s <- loadMatrix f1
  let arah = 3 |>  (map read st :: [Double])
  let vCoord1 = vCF1s
  let vX =  fromRows $ map (f arah) $ toRows vCoord1

  putStrLn $ format "  " (printf "%.6f") vCF1s
  putStrLn "============================="
  putStrLn $ format "  " (printf "%.6f") vX
    where
      f a x = x - a
-}
