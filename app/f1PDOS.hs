#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

import CPVO.IO.Plot.DOS
import CPVO.IO.Plot.Gnuplot.Common
import CPVO.IO.Plot.Gnuplot.Type
import CPVO.IO.Reader.Ecalj.Band
import CPVO.IO.Reader.Ecalj.Util

import Data.List.Split (splitOn)
import Options.Applicative as O
import Data.Semigroup -- <>

import CPVO.IO
import CPVO.IO.Type

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List.Split
import System.Process as SP

import Data.List -- intercalate
import Text.Printf
import Data.Maybe
import System.FilePath.Posix -- takeExtension


main :: IO ()
main = do
  opts@(Opts fOut useOldBw judulUtama yr xr atomOs daftarLengkap) <- execParser withHelp
  let initSetting = defSetting { _titles = judulUtama
                               , _yrange = yr
                               , _xrange = xr
                               , _xylabel = unlines $
                                  "set xlabel 'Energy-E_F (eV)'":
                                  "set ylabel 'Density of States (states/eV.cell)'":[]
                               }
  debugIt "INIT: Opts == " [opts]
  debugIt "INIT: setting == " [initSetting]
  plotWork initSetting fOut (plotTDOSnPDOS useOldBw atomOs)
    $ map (splitOn "#") daftarLengkap
  putStrLn "========DONE======="
    where
      plotTDOSnPDOS a b c d e = do
        h@((s,_):_) <- mapM (\f -> f a b c d e) [plotTDOS]
        --h@((s,_):_) <- mapM (\f -> f a b c d e) [plotTDOS,plotPDOS']
        let res = concat $ map snd h
        return (s,res)



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

withHelp :: ParserInfo Opts
withHelp = info
  (helper <*> optsParser)
  (fullDesc <> (progDesc
    "Density of States (DOS) and Partial DOS Generator for ecalj, please make sure that TARGETDIRS are the last arguments."
  ))
