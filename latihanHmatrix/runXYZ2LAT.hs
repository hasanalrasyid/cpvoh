#!/usr/bin/env stack 
{- stack --install-ghc 
  runghc 
  --package turtle
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

import Turtle                       --
import qualified Control.Foldl as Fold
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified Data.Text.Format as T
import Text.Printf 
import Data.List.Split
import Data.Maybe
import Data.Either
import Data.List
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

shell2list :: MonadIO io => Shell a -> io [a]     
shell2list xx = fold (xx) Fold.list   

main = do
    [fs,latConstant] <- getArgs
    fCart <- readFile fs
    let hasil = map words $
          tail $ tail $ lines fCart
    let coord = fromLists $ map ((map read) . tail) hasil :: Matrix Double
        header = map head hasil
    let latC = read latConstant :: Double
        final = scale (1/latC) coord
{-
    let (gap,awal) = 
          (\(a,b) -> (head b - head a , head a)) $
          head $
          filter (\(a,b) -> (head a * head b)  < 0 ) $
          map (\(x,y)->(x,last y)) $ 
          catMaybes $
          map  uncons $ 
          filter (/=[]) $ 
          splitWhen (\x -> (x!!kolom)>0.01) $ 
          map ((\(a:as) -> a*13.605:as). map (fst) .  rights. (map T.double ). T.words) 
          $ T.lines fDosTot
-}
    putStrLn $ show $ coord 
    putStrLn $ show $ final 
--    putStrLn $ unwords $ map show [gap,awal]
