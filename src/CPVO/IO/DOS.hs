{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module CPVO.IO.DOS (
    pdosA',
    getPDOS,
    getAllPDOS,
    showTotPDOS
  ) where

import CPVO.Numeric
import CPVO.IO

import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List.Split
import Data.List
import Data.Maybe
import Numeric.LinearAlgebra

showTotPDOS :: [String] -> IO ()
showTotPDOS (texFile:jd:jdHead:xr:ymax':wTot:tumpuk:invS:tailer:foldernya:aos) = do
    fCtrl <- T.readFile $ foldernya ++ "/ctrl." ++ tailer
    -------------------------------start calculation------------------------
      -------------------------------generating data------------------------
    let invStat = read invS :: Int
    let ymax = read ymax' :: Double
        [xmin,xmax] = map (read :: String -> Double) $ splitOn ":" xr
      -------------------------------generating DOS data------------------------
    totalDOS <- loadMatrix $ foldernya ++ "/dos.tot." ++ tailer
      -------------------------------integrating DOS data------------------------
    let intgTot = map (\ii -> integrateAll 0
                              $ (++ ([toList $ getY0 ii])) $ takeWhile (\(a:_) -> a <= 0)
                              $ toLists ii) $ map (\i -> totalDOS ¿ [0,i]) [1,2] -- run it on spin [1,2]
    putStrLn $ show intgTot

      -------------------------------integrating PDOS data------------------------
    let ctrlAtoms =
          catMaybes $
          map ( T.stripPrefix "ATOM=" .  head) $
          filter (/=[]) $
          map ( T.words . T.takeWhile (/='#') ) $
          head $
          splitWhen (T.isPrefixOf "SPEC") $
          last $ splitWhen (T.isPrefixOf "SITE")
          $ T.lines fCtrl
        nAtom = length ctrlAtoms
    -- uniqAtoms : [(jumlah,nourutAtom,symbol)]
    let uniqAtoms =
          map (\a -> (length a, snd $ head a, fst $ head a)) $
          groupBy (\a b -> fst a == fst b) $
          zip  ctrlAtoms [1..]
    -- daftarCetak : [(nourut,,jumlah,nourut,symbol)]
    let daftarCetak'  = zip [1..]
                      $ map (\(a,label,b) -> (head $ filter (\(_,_,aa) -> aa == (T.pack a)) uniqAtoms , label, b) )
                      $ map ( (\(a:label:as) -> (a,label,as)) . splitOn ":") aos
        daftarCetak = [ (i,j) | i <- daftarCetak' , j <- [1,2] ]
      -------------------------------generating PDOS data------------------------
    let aoSet = map ( (\(a:l:as) -> (a,l,map ( ((+) (-1)) . read :: String -> Int) as) ) . splitOn ":") aos
    pdosA <- sequence $ (\x ->  [f a | f <- (pdosA' foldernya tailer), a <- x])
              -- ((namaAtom,jdAtom,[intAOs]),[(nourutAtom,namaAtom)])
              -- map ditambah -1 karena input mengikuti gnuplot
              -- input : d kolom 6-10
              -- gnuplot : d kolom 6-10
              -- hmatrix : d kolom 5-9
              $ map (\x@((_,i):_) -> (head $ takeAO i aoSet ,x) )
              $ groupBy (\(_,a:_) (_,b:_) -> (ord a) == (ord b))
              $ zip ([1..]::[Int]) $ map T.unpack ctrlAtoms
      -------------------------------integrating PDOS data------------------------
    let intgPdosA =  map (\(s,j,mP) -> (s,j,integrateAll 0
                                            $ (++ ([toList $ getY0 $ takeColumns 2 mP]))
                                            $ takeWhile (\(a:_) -> a <= 0)
                                            $ toLists $ takeColumns 2 mP  )) pdosA   -- we only consider the first 2 columns, i.e. Energy, PDOS of 1st Atom
-------------------------------generating MarkDown Representation------------------------

    let rIntgAll' =
          rendertable
          $ (:) (splitOn "|" jdHead)
          $ (:) ((:) "Total" $ map (showDouble 2) $ (\[a,b] -> [a,b,a+b]) $ intgTot)
          $ map (\[(_,_,u),(_,j,d)] ->  j : map (showDouble 2) [u,d,u+d])
          $ groupBy (\(_,a,_) (_,b,_) -> a == b)
          $ sortBy (\(_,a,_) (_,b,_) ->  compare a b ) intgPdosA
    let rIntgAll = unlines  [
                            rIntgAll'
                            , "Table: " ++ jd
                            ]
    putStrLn rIntgAll
      -------------------------------generating LaTex Representation------------------------
    resIntAll' <- markdownToTex rIntgAll
--    T.putStrLn $ resIntAll'
    let resIntAll = T.replace "\\}" "}"
                  $ T.replace "\\{" "{" $ T.pack
                  $ unlines [
                            "\\begin{longtable}[]{@{}lSSS@{}}"
                            , unlines $ tail $ lines $ T.unpack resIntAll'
                            ]
--    putStrLn $ show intgPdosA
    T.writeFile texFile resIntAll
    putStrLn "====done===="

