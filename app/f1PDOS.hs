#!/usr/bin/env stack
--stack --resolver lts-14.27 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Data.String
import Data.Either -- partitionEithers
--import CPVO.IO.Plot.DOS
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
import qualified Data.Text.Read as T
import Data.List.Split
import System.Process as SP

import Data.List -- intercalate
import Text.Printf
import Data.Maybe
import System.FilePath.Posix -- takeExtension


setPrintSpin "up" = [1]
setPrintSpin "down" = [2]
setPrintSpin _ = [1,2]

getRange s = map (read :: String -> Double) $ splitOn ":" s

main :: IO ()
main = do
  opts@(Opts fOut spinFlip compactMode noTDOS useOldBw judulUtama printSpin yr xr atomOs daftarLengkap) <- execParser withHelp
  let initSetting = defSetting { _titles = judulUtama
                               , _yrange = yr
                               , _xrange = xr
                               , _printSpin = setPrintSpin printSpin
                               , _flipSpin = spinFlip
                               , _xylabel = unlines $
                                  "set label 'Energy-E_F (eV)' at center":
                                  "set label 'Density of States (states/eV/cell)'":[]
                               , _addenda = Addenda
                                  []
                                  (unlines $ "#set label 'repeated' ":[])
                                  (unlines $
                                    "set label 'Density of States (states/eV/cell)' rotate center at screen 0.05,0.5":
                                    "set format x '% h'":
                                    "set xtics font 'Times New Roman,10' nomirror offset -.15,.6 out":
                                    "set label 'Energy-E_F (eV)' center at graph 0.5,-0.3":[] )
                               }
  debugIt "INIT: Opts == " [opts]
  debugIt "INIT: setting == " [initSetting]
  plotWork initSetting fOut gltGeneratorDOS picGeneratorDOS (plotTDOSnPDOS noTDOS useOldBw atomOs)
    $ map (splitOn "#") daftarLengkap
  putStrLn "========DONE======="
    where
      plotTDOSnPDOS noTDOS a b c d e = do
        h@((s,_):_) <- mapM (\f -> f a b c d e) $ if noTDOS then [plotPDOS]
                                                           else [plotTDOS,plotPDOS]
        --h@((s,_):_) <- mapM (\f -> f a b c d e) [plotTDOS,plotPDOS']
        let res = concat $ map snd h
        let ss = fst $ last h
        debugIt "plotTDOSnPDOS:res: " $ map snd h
        debugIt "plotTDOSnPDOS:res: head " $ head $map snd h
        return (s,res)

gltGeneratorDOS _ NullSetting _ = "gltGeneratorDOS:Err NullSetting"
gltGeneratorDOS tempDir (PlotSetting _ judulUtama _ printSpin yr xr xtics ar _ lbb)
  plotplate =
    let (title:subTitles') = nub $ splitOn "#" judulUtama
        subTitles = if (length plotplate > length subTitles') then "Total":subTitles'
                                                              else subTitles'
        locLabel = "at graph 0.8,0.95 font 'Times New Roman Bold,10'"
        newxticks = if (null xtics) then ""
                                    else concat ["set xtics (",xtics,")"]
        numPlots = show $ length subTitles
     in unlines [ genTOP tempDir [numPlots,xr,yr,"default # --poskey-- "]
                , "#set title '" ++ title ++ "'"
                , unlines
                    $ intersperse noMidThings
                    $ addLabels lbb
                    $ zipWith (\s p -> unlines
                                        $ ("set label '" ++ s ++ "' " ++ locLabel):
                                          unwords [ar,plotInit,p]:[]
                              ) subTitles plotplate
                , endMultiplot
                ]

addLabels (Addenda top mid btm) ls@(i:is) =
  let topAdd = unlines $ "#topAdd":top:[]
      midAdd = unlines $ "#midAdd":mid:[]
      btmAdd = unlines $ "#btmAdd":btm:[]
      ims = map (midAdd ++) $ init is
      ii = last is
   in [topAdd ++ midAdd ++ i] ++ ims ++ [midAdd ++ btmAdd ++ ii]
addLabels NullAddenda is = "#NullAddenda":is
addLabels _ [] = ["#error: Empty is @addLabels"]

noMidThings = unlines $ "unset ylabel":
                        "unset title":
                        "unset label":
                        []


picGeneratorDOS plotter dirList (iniSetting,res) = do
  debugIt "=====picGeneratorDOS init ====" ""
  (plotSetting, generatedPlots) <- plotter (head dirList) 1 (iniSetting,[])
  debugIt "picGeneratorDOS:iniSetting: " [plotSetting]
  debugIt "picGeneratorDOS: generatedPlots: " $ map genPlots generatedPlots
  debugIt "=====picGeneratorDOS end  ====" ""
  return (plotSetting,map genPlots generatedPlots)
  --return (plotSetting,["---result1 of picGeneratorDOS","result2 of picGeneratorDOS"])
  where genPlots (a,b) = T.unpack $ T.intercalate "," [a,b]

plotPDOS :: Bool
         -> String
         -> [String]
         -> Integer
         -> (PlotSetting, [(T.Text, T.Text)])
         -> IO (PlotSetting, [(T.Text, T.Text)])
plotPDOS  _        _      []                   _       res = return res
plotPDOS  useOldBw atomOs (daftarLengkap:sisa) colorId (iniSetting,res) = do
  putStrLn "====plotPDOS======="
  let (foldernya:spinnya:legend:_) = splitOn ":" daftarLengkap
  fileCtrl <- inshell2text $ "ls " ++ foldernya ++ "/ctrl.*"
  putStrLn $ "====" ++ show fileCtrl ++ "======" ++ show foldernya
  fCtrl <- T.readFile $ T.unpack $ head fileCtrl
--  let invStat = if (invS == "flipSpin") then (-1) else 1
  let invStat = _flipSpin iniSetting
  putStrLn $ show spinnya
  putStrLn $ show legend
  -- aos :: Ni:Ni#3d:6:7:9:8:10-Co:Co#3d:6:7:8:9:10-O:O#2p:3:4:5
  let theTailer = tail $ takeExtension $ T.unpack $ head fileCtrl
  ctrlAtoms <- readCtrlAtoms theTailer foldernya
  debugIt "===ctrlAtoms: " ctrlAtoms
  -- aos :: Ni:Ni#3d: 6: 7: 9: 8:10-Co:Co#3d:6:7:8:9:10-O:O#2p:3:4:5
  -- alt :: Ni:Ni#3d@d             -Co:Co#t2g@d1:d2-O:O#2p@p
  let aoSet = getAOSet ctrlAtoms atomOs
  let newSetting = iniSetting {_titles = intercalate "#" $ (:) (_titles iniSetting) $ map labelAO aoSet}
--  let requestedAtOrbs = genCtrlAtomicAOs aoSet ctrlAtoms
  debugIt "daftaratomOs: " aoSet

  let [resSpin1,resSpin2] = map T.pack $ map (susunTot foldernya theTailer invStat ) ([1,2] :: [Int])
  let dCetak = [ (s,u,j,a) | (u,as) <- zip [1..]
                                $ groupBy (\a b -> labelAO a == labelAO b && intAOs a == intAOs b) aoSet
               , let j = length as
               , let a = head as
               , s <- [1,2]
               ]

  let (rSpin1,rSpin2) = partition (T.isInfixOf ".isp1.") $ map T.pack $ map (drawOrb "dos" foldernya theTailer invStat) dCetak
  debugIt "plotPDOS:res1: " $ zip rSpin1 rSpin2
  debugIt "plotPDOS:iniSetting: " [iniSetting]
  return $ (newSetting,zip rSpin1 rSpin2)
--  plotPDOS useOldBw atomOs sisa (colorId+1) (newSetting,(resSpin1,resSpin2):res)

data Opts = Opts {
    _fOut       :: String,
    _spinFlip   :: Bool,
    _compactMode :: Bool,
    _totalDOS   :: Bool,
    _useOldBw   :: Bool,
    _judulUtama :: String,
    _pSpin      :: String,
    _yr         :: String,
    _xr         :: String,
    _atomOs     :: String,
    _targetDir  :: [String]
                 } deriving Show

optsParser :: Parser Opts
optsParser = Opts
             <$> strOption ( long "out" <> short 'o' <> metavar "OUTPUT" <>
                           help "target output file" <> value "test")
             <*> switch ( long "flip-spin" <> short 'f'
                      <> help "Flip up and down spin, default is False")
             <*> switch ( long "compact-mode" <> short 'c'
                      <> help "Put all graphs in single frame")
             <*> switch ( long "no-tdos" <> short 'd'
                      <> help "Remove Total DOS")
             <*> switch ( long "oldbw" <> short 'b'
                      <> help "Using old BandWeight files")
             <*> strOption (long "titles" <> short 't' <>
                            metavar "SUBTITLES" <>
                            help "Titles of each Figures")
             <*> strOption ( long "print-all-spin" <> short 'a' <>
                            metavar "[up/down/all]" <>
                            help "Spin state to be printed, ex. \"up\"" <>
                            value "all")
             <*> strOption ( long "yrange" <> short 'y' <>
                            metavar "MIN:MAX" <>
                            help "Y-range of the figure, ex. \"-2.5:7.3\"")
             <*> strOption ( long "xrange" <> short 'x' <>
                            metavar "MIN:MAX" <>
                            help "X-range of the figure, ex. \"-8:3\"")
             <*> strOption ( long "orbitals" <> short 'r' <>
                            metavar "NUMATOM/NAMEATOM:[LABEL]@orb1[:orb2[..]][-..]" <>
                            help "List of atomic orbitals ex. 7@p-Ni:Ni_label@dx2My2:d1 for p orbital of 7th atom with default label and d1+dx2My2 orbital for all Ni atom with label Ni_label." <> value "")
             <*> (many $ argument str (metavar "TARGETDIRS"))

withHelp :: ParserInfo Opts
withHelp = info
  (helper <*> optsParser)
  (fullDesc <> (progDesc
    "Density of States (DOS) and Partial DOS Generator for ecalj, please make sure that TARGETDIRS are the last arguments."
  ))

plotter1Pic :: Bool
               -> String
               -> [String]
               -> Int
               -> (PlotSetting , [(T.Text, T.Text)])
               -> IO (PlotSetting ,[(T.Text, T.Text)])
plotter1Pic  _        _      []                   _       res = return res
plotter1Pic  useOldBw atomOs (daftarLengkap:sisa) colorId (iniSetting,res) = do
  let (foldernya:spinnya:legend:_) = splitOn ":" daftarLengkap
  bandFiles <- inshell2text $ unwords ["ls",concat[foldernya,"/bnd*spin",spinnya]]
  let daftarFolder = ":" ++ intercalate "@" [legend,spinnya,foldernya]

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
        daftarAOJSF = zip ([1..] :: [Int]) $ concat $ map (\a -> zip daftaratomOs $ repeat a) daftarJudulSpinFolders

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

delta :: Bool -> b -> b -> b
delta x y z = if x then y else z

caller :: T.Text
caller = T.unlines  [ "callme with : genPDOSAtomicOrbital.hs [Atom:Orbital]                tailer    folder"
                    , "ex.         : genPDOSAtomicOrbital.hs 'O NiTd:2:3:4:5 CoTd:2:3:4:5' nico2o4   nico2o4.invB.0GGA"
                    ]
tampilkan :: Show a => [a] -> IO ()
tampilkan [] = putStrLn "===================="
tampilkan (a:as) = do
        putStrLn $ show a
        putStrLn $ "++++"
        tampilkan as

  -- alt :: Ni:Ni#3d@d             -Co:Co#t2g@d1:d2-O:O#2p@p
getAOSet ctrlAtoms s =
  let r1 = map (splitOn "@") $ splitOn "-" s
--      res = map proc1 r1
      defineA at = procAt $ map T.pack $ splitOn ":" at
      procAt (aOrn:l:[]) = toDec aOrn l
      procAt (aOrn:[]) = toDec aOrn ""
      procAt x = Left $ "getAOSet: procAt: error form" ++ show x
      toDec t lb = case T.decimal t of
                     Right (v,_) -> Right (v,"",lb)
                     Left x ->  Right $ (0, t, lb)

      defineOrbs o =  let (fOrb,okOrb) = partitionEithers $ map getOrbital $ splitOn ":" o
                       in if null fOrb then Right $ concat okOrb
                                       else Left "Error @getAOSet: undefined orbitals found"
      proc1 (atom:orbs:_) = (defineA atom, defineOrbs orbs)
      proc1 _ = (Left "error @getAOSet", Left "error @getAOSet: error proc1")
   in [ AO n sss ll is | let lt = zip ctrlAtoms ([1..] :: [Int])
      , (Right (i,ss,l), is) <- map proc1 r1
      , let iss = case is of
                    Left _ -> Left "getAOSet: res: error unknown iss"
                    x -> x
      , (sss,n) <- filter (\(nn,ii) -> nn == ss || ii == i) lt
      , let ll = if T.null l then T.unpack sss else T.unpack l
      ]

instance IsString ErrCPVO where
  fromString s = ErrCPVO $ "ErrCPVO: " ++ s

instance IsString (PlotSetting,[String]) where
  fromString s = (NullSetting,["Err: No PlotSetting implemented fromString"])

plotTDOS :: Bool
               -> String
               -> [String]
               -> Integer
               -> (PlotSetting , [(T.Text, T.Text)])
               -> IO (PlotSetting ,[(T.Text, T.Text)])
plotTDOS  _        _      []                   _       res = return res
--plotStatementDOS (jd:xr:ymax':wTot:_:invS:tailer:foldernya:aos) = do
plotTDOS  useOldBw atomOs (daftarLengkap:sisa) colorId (iniSetting,res) = do
  putStrLn "====plotTDOS======="
  let (foldernya:spinnya:legend:_) = splitOn ":" daftarLengkap
  fileCtrl <- inshell2text $ "ls " ++ foldernya ++ "/ctrl.*"
  putStrLn $ "====" ++ show fileCtrl ++ "======" ++ show foldernya
  fCtrl <- T.readFile $ T.unpack $ head fileCtrl
--  let invStat = if (invS == "flipSpin") then (-1) else 1
  let invStat = _flipSpin iniSetting
  putStrLn $ show spinnya
  putStrLn $ show legend
  -- aos :: Ni:Ni#3d:6:7:9:8:10-Co:Co#3d:6:7:8:9:10-O:O#2p:3:4:5
  let theTailer = tail $ takeExtension $ T.unpack $ head fileCtrl
  ctrlAtoms <- readCtrlAtoms theTailer foldernya
  debugIt "===ctrlAtoms: " ctrlAtoms
  -- aos :: Ni:Ni#3d: 6: 7: 9: 8:10-Co:Co#3d:6:7:8:9:10-O:O#2p:3:4:5
  -- alt :: Ni:Ni#3d@d             -Co:Co#t2g@d1:d2-O:O#2p@p
  let aoSet = getAOSet ctrlAtoms atomOs
--  let requestedAtOrbs = genCtrlAtomicAOs aoSet ctrlAtoms
  debugIt "daftaratomOs: " aoSet
  let newSetting = iniSetting {_titles = intercalate "#" $ (:) (_titles iniSetting) $ nub $ map labelAO aoSet}
  let [resSpin1,resSpin2] = map T.pack $ map (susunTot foldernya theTailer invStat ) ([1,2] :: [Int])
  plotTDOS useOldBw atomOs sisa (colorId+1) (newSetting,(resSpin1,resSpin2):res)

insertText :: String -> String -> String
insertText t p = unlines [t,p]

insertLabel :: String -> String -> String -> String
insertLabel l a p = unlines [ concat [ "set label '",l,"' ",a], p]

drawOrb :: T.Text
        -> String
        -> String
        -> Bool
        -> (Int, Int, Int, AtOrb)
        -> String
drawOrb job foldernya tailer' invStat
  (spin, urutan, jumlah, AO nomor _ _ (Right lAOs)) =
    let lOrbitals = (++) "$" $ intercalate "+$" $ map show (lAOs :: [Integer])
     in unwords [ Text.Printf.printf "'%s/%s.isp%d.site%03d.%s' u ($1*rydberg):((%s)*%d*(%d)*(%d)/rydberg ) w l ls %d notitle"
                  (T.pack foldernya)
                  job
                  spin
                  nomor
                  (T.pack tailer')
                  lOrbitals
--                  ("$" ++ (intercalate "+$" $ delta (listOrbital /= []) listOrbital $ map show ([2..26] :: [Int])))
                  jumlah
                  (delta invStat (-1) 1 :: Int)
                  (delta (spin < 2) 1 (-1) :: Int)
                  urutan
                ]
drawOrb _ _ _ _ _ = "sin(x) #error drawOrb"


susunOrbs :: T.Text
                -> String
                -> String
                -> Bool
                -> ((Int,((Int, Int, T.Text),String,[String])),Int) -- DaftarCetak
                -> String
--susunOrbs job foldernya tailer invStat ((urutan,((jumlah,nomor,nama),judul,listOrbital)),spin) = unwords [
susunOrbs job foldernya tailer' invStat ((urutan,((jumlah,nomor,_),_,listOrbital)),spin) = unwords [
                                        Text.Printf.printf "'%s/%s.isp%d.site%03d.%s' u ($1*rydberg):((%s)*%d*(%d)*(%d)/rydberg ) w l lw 2 ls %d notitle"
                                          (T.pack foldernya)
                                          job
                                          spin
                                          nomor
                                          (T.pack tailer')
                                          ("$" ++ (intercalate "+$" $ delta (listOrbital /= []) listOrbital $ map show ([2..26] :: [Int])))
                                          jumlah
                                          (delta invStat (-1) 1 :: Int)
                                          (delta (spin < 2) 1 (-1) :: Int)
                                          urutan
                                          ]


susunTot :: String -> String -> Bool -> Int -> String
susunTot foldernya tailer' flipSpin spin =
  unwords [ Text.Printf.printf
              "'%s/dos.tot.%s' u ($1*rydberg):($%d *( %d ) *( %d ) / rydberg ) w l lw 1 lc rgb 'black' notitle "
              (T.pack foldernya)
              (T.pack tailer')
              (delta (spin < 2) 2 (3) :: Int)
              (delta flipSpin (-1) 1 :: Int)
              (delta (spin < 2) 1 (-1) :: Int)
          ]



