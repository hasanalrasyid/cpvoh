#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

module Main where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import System.Environment
import Text.Printf (printf)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Machine

main :: IO ()
main = do
  n <- runT pipeline
  -- Remember that runT collects outputs in a list
  print (head n)
  where pipeline = lineSource ~> skipHeader ~> countCommas ~>
                   filterBad ~> count

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
