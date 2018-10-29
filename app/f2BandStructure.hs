#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

--import CPVO.IO.Plot.Band

import CPVO.IO -- inshell2text
import System.Environment (getArgs)
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.List -- intercalate

--import qualified Control.Foldl as Fold
--import System.Environment (getArgs)
--import qualified Data.Text as T
--import qualified Data.Text.IO as T
--import qualified Data.Text.Format as T
--import Text.Printf
--import Data.List.Split
--import Data.Maybe
import Data.Either -- rights
import Numeric.LinearAlgebra.Data -- toLists, fromLists
import Data.Maybe -- fromJust


main :: IO ()
main = do
  allArgs <- getArgs
  {-
  let testArgs1 = [ "fig2"
                 , "-9:2"
                 , "55" , "o" ,"1", "top:right", "T"
                 , "extendedNiCo2O4.normal/nico2o4.1G0"
                 , "Ni:Ni#3d:6:7:9:8:10", "Co:Co#3d:6:7:8:9:10", "O:O#2p:3:4:5"
                 ]
  let testArgs = [ "QSGW_{0}"
                 , "-9:2", "55", "T"
                 , "o", "1"
                 , "nico2o4"
                 , "extendedNiCo2O4.normal/nico2o4.0GGA"
                 ,"Ni:Ni#3d:6:7:9:8:10", "Co:Co#3d:6:7:8:9:10", "O:O#2p:3:4:5"
                 ]
  -}
  plotBand allArgs
  putStrLn "========beres======="

plotBand :: [String] -> IO ()
plotBand (fOut:useOldBw:judulUtama:yr:atomOs:daftarLengkap) = do
  putStrLn "===start: plotBand===="
  let ender = unlines [ "unset multiplot"
                      , "system \"cd plots && epstopdf hasil.eps && pdftocairo -r 150 -singlefile -jpeg hasil.pdf tmp && convert tmp.jpg -rotate 90 hasil.jpg && rm -f tmp.jpg\""
                      ]
  let isi = unlines [ "set datafile missing '-'"
                    , "plot 0 lt -1 lc rgb 'black' title ''"
                    ]
  putStrLn ender


  putStrLn $ unlines daftarLengkap
  let inp@(foldernya:spinnya:legend:_) = splitOn ":" $ head daftarLengkap
  bandFiles <- inshell2text $ unwords ["ls",concat[foldernya,"/bnd*spin",spinnya]]
  --batasAtasX <- fmap (head . (drop 1) . T.words . head ) $ inshell2text $ unwords ["tail -1",T.unpack lastBandFile]
  batasAtasX <- fmap (head . (drop 1) . last . filter (not . null). map T.words) $ inshell2text $ unwords ["tail -2",T.unpack $ last bandFiles]
  let daftarFolder = ":" ++ intercalate "@" [legend,spinnya,foldernya]

-- for i in $(ls $foldernya/bnd0*spin$spinnya);do
  let subjudul = "title ''"
  let plotplate = T.intercalate ", " $ map genPlotPlate $ map (\x -> T.concat["'",x,"'"]) bandFiles
  valBand@(valBandX:valBandY:_) <- fmap (words . T.unpack . head) $ inshell2text $ unwords ["cat",concat [foldernya,"/bnd*spin*",spinnya],"|sed -e '/^#/d' |awk '{if ($3>0.1) print $2,$3}'|sort -k 2n|head -1"]

  condBand@(condBandX:condBandY:_) <- fmap (words . T.unpack . head) $ inshell2text $ unwords ["cat", concat[foldernya,"/bnd*spin*",spinnya],"| sed -e '/^#/d' |awk '{if ($3<=0) print $2,$3}'|sort -k 2nr -u|sed -e '/^ *$/d'|head -1"]
  putStrLn $ "===valBand==" ++ show (valBand :: [String])
  putStrLn $ "===conBand==" ++ show (condBand :: [String])
  --allBand <- fmap (runGAPband . head) $ inshell2text $ "cat " ++ concat [foldernya,"/bnd*spin*",spinnya]
  gapCoordBandGap <- fmap runGAPband $ inshell2text $ "cat " ++ concat [foldernya,"/bnd*spin*",spinnya]
  let bandGap = last $ words gapCoordBandGap
  putStrLn $ "===allBand=== " ++ show (bandGap :: String)
  let gapArrow@(gapnya,arrow) =
        if (bandGap == "0")
           then (Nothing,Nothing)
           else (,) (Just $ "0:" ++ bandGap ++ "@" ++ valBandY)
                    (Just $ unlines [ concat ["label sprintf ('{/Symbol D}=%.2feV',"
                                      , bandGap
                                      , ") at (" ++ valBandX ++ "-0.55),"
                                      ,"(" ++ valBandY ++ "-0.4) font ',12'"
                                      ]
                             , concat [ "set arrow from "
                                      , valBandX ++ "," ++ condBandY
                                      , " to " ++ valBandX ++ "," ++ valBandY
                                      , " heads noborder lw 2 lc rgb 'blue'"
                                      ]
                             ]
                    )
  putStrLn $ show gapArrow
  putStrLn $ "perintahDOS == " ++ unwords [
    "genPDOSvert.hs ", atomOs, fromJust gapnya, spinnya, daftarFolder]
    {-
  putStrLn $ show plotplate
  putStrLn $ show batasAtasX
  putStrLn $ show daftarFolder
  putStrLn $ show inp
-}
  putStrLn "===end  : plotBand===="

plotBand _ = putStrLn "Error: complete arguments needed"

genPlotPlate bnd = T.concat [ bnd , T.pack " u ($2>6.08991818?0.4+$2:$2):($2>6.08991818&&$2<6.09991818?1/0:$3) with line lc rgb 'black' title ''"]

-- import Turtle                       --


runGAPband bndSpin =
  let (valencePoint, conductionPoint) =
        (\(a,b) -> (last a,head b)) $
        break (\[_,a] -> a > 0) $
        Data.List.sortBy (\[_,a] [_,b] -> if a < b then LT else GT ) $
        toLists $
        (¿ [1,2]) . fromLists $
        filter (/=[]) $  -- kolom $2 x (k-point) -- $3 y Energy
--      map ( T.words ) $ -- mengubah column based text number jadi [[Double]]
        map ( (map fst) . rights . (map T.double) . T.words ) $ -- mengubah column based text number jadi [[Double]]
        filter (not . T.isPrefixOf "#") $
        bndSpin

  in if (last conductionPoint - last valencePoint < 0.01)  then "0 0 0"
                                                           else unwords $ map show  [head valencePoint, head conductionPoint, last conductionPoint - last valencePoint]

