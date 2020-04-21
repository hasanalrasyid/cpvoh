#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

import CPVO.IO.Plot.Gnuplot.Common
import CPVO.IO.Plot.Gnuplot.Type

import Data.List.Split (splitOn)
import Options.Applicative as O
import Data.Semigroup -- <>

main :: IO ()
main = do
  (Opts fOut useOldBw judulUtama yr atomOs daftarLengkap) <- execParser withHelp
  let initSetting = defSetting { _titles = judulUtama
                               , _yrange = yr
                               , _xylabel = unlines $ "set xlabel 'Wave Vector'":
                                                      "set ylabel 'Energy-E_F (eV)'":[]
                               }
  putStrLn $ show initSetting
  putStrLn $ fOut
  putStrLn $ show useOldBw
  putStrLn $ atomOs

  plotWork initSetting fOut genTEMPGLT genAllPics
    (plotter1Pic useOldBw atomOs)
    $ map (splitOn "#") daftarLengkap
  putStrLn "========DONE======="

data Opts = Opts {
    _fOut       :: String,
    _useOldBw   :: Bool,
    _judulUtama :: String,
    _yr         :: String,
    _atomOs     :: String,
    _targetDir  :: [String]
                 } deriving Show

optsParser :: Parser Opts
optsParser = Opts
             <$> strOption (long "out" <> short 'o' <> metavar "OUTPUT" <>
                          help "target output file" <> value "test")
             <*> switch (long "oldbw" <> short 'b'
                            <> help "Using old BandWeight files")
             <*> strOption (long "titles" <> short 't' <>
                            metavar "SUBTITLES" <>
                            help "Titles of each Figures" <>
                            value "")
             <*> strOption  ( long "yrange" <> short 'y' <>
                            metavar "MIN:MAX" <>
                            help "Y-range of the figure, ex. \"-2.5:7.3\"")
             <*> strOption (long "orbitals" <> short 'r' <> metavar "ORBITALS"
                            <> help "List of atomic orbitals" <> value "")
             <*> (many $ argument str (metavar "TARGETDIRS"))

{-
         "~/kanazawa/dev/cpvoh/app/f2 -o outputfile -b -y \"-9.5:2\" ext.nico2o4.normal/nico2o4.6G10:1:\"11G20.Majority\"#ext.nico2o4.normal/nico2o4.6G10:2:\"11G20.Minority\" ext.nico2o4.normal/nico2o4.1G0:1:\"G0.Majority\"#ext.nico2o4.normal/nico2o4.1G0:2:\"G0.Minority\"":
-}
withHelp :: ParserInfo Opts
withHelp = info
  (helper <*> optsParser)
  (fullDesc <> (progDesc
    "Band Structure Generator for ecalj, please make sure that TARGETDIRS are the last arguments."
  ))


plotter1Pic :: Bool
               -> String
               -> [String]
               -> Int
               -> (PlotSetting , [(T.Text, T.Text)])
               -> IO (PlotSetting ,[(T.Text, T.Text)])
plotter1Pic  _        _      []                   _       res = return res
plotter1Pic  useOldBw atomOs (daftarLengkap:sisa) colorId (iniSetting,res) = do
  putStrLn $ "===plotter1Pic" ++ show daftarLengkap
  let (foldernya:spinnya:legend:_) = splitOn ":" daftarLengkap
  bandFiles <- inshell2text $ unwords ["ls",concat[foldernya,"/bnd*spin",spinnya]]
  let daftarFolder = ":" ++ intercalate "@" [legend,spinnya,foldernya]
  putStrLn $ (++) "===" $ unwords ["ls",concat[foldernya,"/bnd*spin",spinnya]]
  putStrLn $ "===" ++ show daftarFolder ++ show bandFiles ++ spinnya
  let generatedPBAND = T.intercalate ", " $ map (genPlotPlate colorId) $ map (\x -> T.concat["'",x,"'"]) bandFiles
  valBand@(valBandX:valBandY:_) <- fmap (words . T.unpack . head) $ inshell2text $ unwords ["cat",concat [foldernya,"/bnd*spin*",spinnya],"|sed -e '/^#/d' |awk '{if ($3>0.1) print $2,$3}'|sort -k 2n|head -1"]
  (_:condBandY:_) <- fmap (words . T.unpack . head) $ inshell2text $ unwords ["cat", concat[foldernya,"/bnd*spin*",spinnya],"| sed -e '/^#/d' |awk '{if ($3<=0) print $2,$3}'|sort -k 2nr -u|sed -e '/^ *$/d'|head -1"]
  putStrLn $ "===valBand==" ++ show (valBand :: [String])
  gapCoordBandGap <- fmap runGAPband $ inshell2text $ "cat " ++ concat [foldernya,"/bnd*spin*",spinnya]
  let bandGap = last $ words gapCoordBandGap
  putStrLn $ "===allBand=== " ++ bandGap
  let ar = genArrow bandGap valBandX valBandY condBandY
--  putStrLn $ show gapArrow
--  putStrLn $ "perintahDOS Removed == " ++ unwords [
--    "genPDOSvert.hs ", atomOs, fromJust gapnya, spinnya, daftarFolder]
  generatedFATBAND <- genPBAND useOldBw spinnya "0" atomOs [daftarFolder]
  putStrLn $ show generatedFATBAND
  (xrangeatas,tck) <- genBandTicks foldernya
  putStrLn $ "========" ++ show xrangeatas ++ "=====" ++ show tck
  putStrLn $ "=== FINISHED PROCESSING : " ++ daftarLengkap
  plotter1Pic useOldBw atomOs sisa (colorId+1) ((iniSetting { _xrange = ("0.0:" ++ xrangeatas), _ticks = tck, _arrow = ar}),(generatedPBAND,generatedFATBAND):res)

genPBAND :: Bool -> String -> String -> String -> [String] -> IO T.Text
genPBAND   _      _   _       []      _         = return ""
genPBAND   useOldBw  _   invStat atomNos foldernya = do
    let daftaratomOs =  map (splitOn "@") $ splitOn "-" atomNos
        daftarJudulSpinFolders = filter (/=[""]) $ map (splitOn "@") $ splitOn ":" $ unwords foldernya
        daftarAOJSF = zip ([1..] :: [Integer]) $ concat $ map (\a -> zip daftaratomOs $ repeat a) daftarJudulSpinFolders

    let err'' = map ( \(_,([o,a],[_,s,folder'])) -> concat [ "cd ", folder', "; BandWeight.py PROCAR.",cekSpin s invStat "1" "UP" "DN" , " ",a, " ", o]) daftarAOJSF
    if (useOldBw == True) then mapM_ system_ err''
                      else return ()

{--
  (\$2>6.08991818?0.4+\$2:\$2):(\$2>6.08991818&&\$2<6.09991818?1/0:\$3)
--}
    return $ T.pack $ concat $ intercalate [", "]
      $ map ( \(i,([o,a],[j,s,folder])) -> ["'",folder,"/bw.",a,".",o,".PROCAR.",cekSpin s invStat "1" "UP" "DN" ,".dat' u ($1>6.09971818?0.4+$1:$1):2:($1>6.08991818&&$1<6.09991818?0:$3*3) ls ",show i," ps variable title '",concat $ intersperse "." [o,a,j],"'"] :: [String] )
      $ daftarAOJSF

genBandTicks :: String -> IO (String,String)
genBandTicks foldernya = do
    lTicks'' <- inshell2text $ unwords ["tail -n2  ", foldernya ++ "/bnd*spin1|sed -e '/^ $\\|==/d'|sort -u|awk '{print $2}' "]
    namaTicks'' <- inshell2text $ unwords ["cat ",foldernya ++ "/syml.* "]
    let lTicks = (:) "0.0" $ map T.unpack $ filter (not . T.null) lTicks''
    let namaTicks =  map (\a -> if a == "Gamma" then "{/Symbol G}" else a) $ (map head $ filter (/=[]) $ map (snd . (splitAt 7) . words . T.unpack)  namaTicks'')
    return $ (,) (last lTicks)
           $ unwords $ intersperse "," $ map (\(a,b) -> unwords ["'"++a++"'",b]) $ zip  namaTicks lTicks
