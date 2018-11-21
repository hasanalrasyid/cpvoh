#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

import CPVO.IO.Plot.DOS
import CPVO.IO.Plot.Gnuplot.Common
import CPVO.IO.Plot.Gnuplot.Type

import Data.List.Split (splitOn)
import Options.Applicative as O
import Data.Semigroup -- <>

main :: IO ()
main = do
  (Opts fOut useOldBw judulUtama yr xr atomOs daftarLengkap) <- execParser withHelp
  let initSetting = defSetting { titles = judulUtama, yrange = yr, xrange = xr }
  plotWork initSetting fOut (plotTDOSnPDOS useOldBw atomOs)
    $ map (splitOn "#") daftarLengkap
  putStrLn "========DONE======="
    where
      plotTDOSnPDOS a b c d e = do
        h@((s,_):_) <- mapM (\f -> f a b c d e) [plotTDOS,plotTDOS]
        let res = concat $ map snd h
        return (s,res)

  {-
mainBand :: IO ()
mainBand = do
  (Opts fOut useOldBw judulUtama yr atomOs daftarLengkap) <- execParser withHelp
  let initSetting = PlotSetting judulUtama yr "" "" ""
  plotWork initSetting fOut (plotter1Pic useOldBw atomOs)
    $ map (splitOn "#") daftarLengkap
  putStrLn "========DONE======="
-}

data Opts = Opts {
    _fOut       :: String,
    _useOldBw   :: Bool,
    _judulUtama :: String,
    _yr         :: String,
    _xr         :: String,
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
             <*> strOption  ( long "xrange" <> short 'x' <>
                            metavar "MIN:MAX" <>
                            help "X-range of the figure, ex. \"-8:3\"")
             <*> strOption (long "orbitals" <> short 'r' <> metavar "ORBITALS"
                            <> help "List of atomic orbitals ex. p@7-dx2My2@11-dz2@11" <> value "")
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
