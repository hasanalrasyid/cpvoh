#!/usr/bin/env stack
--stack --resolver lts-2.22 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

module Main where

import Lib
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import System.Environment
import Text.Printf (printf)

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
