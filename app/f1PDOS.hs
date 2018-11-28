#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

--import CPVO.IO.Plot.DOS
import CPVO.IO.Plot.Gnuplot.Common
import CPVO.IO.Plot.Gnuplot.Type
import CPVO.IO.Reader.Ecalj.Band
import CPVO.IO.Reader.Ecalj.Common

import Data.List.Split (splitOn)
import Options.Applicative as O
import Data.Semigroup -- <>

import CPVO.IO

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
  (Opts fOut useOldBw judulUtama yr xr atomOs daftarLengkap) <- execParser withHelp
  let initSetting = defSetting { _titles = judulUtama, _yrange = yr, _xrange = xr, _xylabel = unlines $ "set xlabel 'Energy-E_F (eV)'":
                         "set ylabel 'Density of States (states/eV.cell)'":[] }
  plotWork initSetting fOut (plotTDOSnPDOS useOldBw atomOs)
    $ map (splitOn "#") daftarLengkap
  putStrLn "========DONE======="
    where
      plotTDOSnPDOS a b c d e = do
        h@((s,_):_) <- mapM (\f -> f a b c d e) [plotTDOS,plotPDOS]
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
    "Density of States (DOS) and Partial DOS Generator for ecalj, please make sure that TARGETDIRS are the last arguments."
  ))
--{-# LANGUAGE TypeApplications #-}
--{-# LANGUAGE ScopedTypeVariables #-} -- untuk let x :: Int = 5

--plotPDOS plotStatementDOS_args@(jd:xr:ymax':wTot:tumpuk:invS:tailer:foldernya:aos) = do
  --invStat : spin inversal... it means switching between up n down spin
  --          flipSpin : flip it
  --          otherwise: keep it straight
  --
--plotPDOS :: [String] -> IO ()

plotPDOS' (fOut:xr:yr:over:invStat:poskey':total:foldernya:daftarOrbital) = do
  putStrLn "====start: CPVO.IO.Plot.DOS===="
-------------draft---------------------------------------
  let poskey = unwords $ splitOn ":" poskey'
  tailer' <- fmap (T.unpack . last . T.splitOn "." . head ) $ inshell2text $ unwords [ "ls", foldernya ++ "/dos.tot.*" ]
  putStrLn $ show poskey
  -- tailer="$(ls "${dirs[0]}" |grep dos.tot | awk -F '.' '{print $NF}' )"
  -- over=$3
  -- mkdir -p plots
  let ender = genEnder
  let akhiran = unlines [ ender
                        , "system 'cd plots && rm -f tmp*jpg && for i in {eps,pdf,png}; do mv hasil.$i " ++ fOut ++ ".$i; done '"
                        ]
  let plotplate = "set format x '% h'; set xtics format '' nomirror ; unset xlabel; unset ylabel "
  let topTitle' = (last $ T.splitOn "." $ T.pack foldernya)
  let topTitle = case topTitle' of
                      "0GGA" -> "GGA(PBE)"
                      _ -> concat [ "QSGW_{"
                                  , T.unpack $ last $ T.splitOn "G" topTitle'
                                  , "}"
                                  ]
      --generator="f1.genPDOSAtomicOrbitalTot.hs $topTitle $xr $yr $total $over $invStat $tailer $foldernya $daftarOrbital"

  plotplate1 <- plotStatementDOS (topTitle:xr:yr:total:over:invStat:tailer':foldernya:daftarOrbital)
  putStrLn "==========================================="
  T.writeFile "temp.glt" $ T.pack $ unlines [ genTOP [xr,yr,poskey]
                     , plotplate
                     , plotplate1
                     , akhiran
                     ]
  _ <- SP.system "gnuplot temp.glt"
    -- #---#dimensi=$(convert plots/hasil.jpg -fuzz 5% -transparent white sparse-color:-|sed -e 's/ /\n/g'|awk -F ',' 'BEGIN{a=0; b=0;aa=10000;bb=10000}{if (a<$1) a=$1; if ($1<aa) aa=$1;  if (b<$2) b=$2; if (bb>$2) bb=$2 }END{print a-(10-a%10)"x"b-bb+(10-b%10)"+"aa-(30+aa%10)"+"bb-(10-aa%10)}')
    -- #---#convert plots/hasil.jpg -crop $dimensi plots/hasil.jpg
    -- #---##convert plots/hasil.jpg -pointsize 24 -font "monofur" label:'Energy (eV)' -gravity Center -append plots/hasil.jpg
    -- #---##convert plots/hasil.jpg -gravity West -font monofur -pointsize 24 -draw 'rotate -90 text 0,20 "DOS (states/eV)"' plots/hasil.jpg

  ------------------------------
  putStrLn "====finish: CPVO.IO.Plot.PDOS===="
plotPDOS' _ =
  putStrLn "====Error: CPVO.IO.Plot.PDOS : incomplete Arguments===="

  {-
module CPVO.IO.Plot.Band
  where


import CPVO.IO -- inshell2text
import Data.List.Split (splitOn)
import qualified Data.Text as T
-}

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

  {-
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module CPVO.IO.Plot.Gnuplot.DOS
  where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List.Split
import Data.List
import Data.Maybe
-}

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

plotPDOS :: Bool
               -> String
               -> [String]
               -> Int
               -> (PlotSetting , [(T.Text, T.Text)])
               -> IO (PlotSetting ,[(T.Text, T.Text)])
plotPDOS  _        []     _                    _       res = return res
plotPDOS  _        _      []                   _       res = return res
plotPDOS  useOldBw atomOs (daftarLengkap:sisa) colorId (iniSetting,res) = do
  putStrLn "====plotPDOS======="
  let (foldernya:spinnya:legend:_) = splitOn ":" daftarLengkap
  fileCtrl <- fmap head $ inshell2text $ "ls " ++ foldernya ++ "/ctrl.*"
  putStrLn $ "====" ++ show fileCtrl ++ "======" ++ show foldernya
--  fCtrl <- T.readFile $ T.unpack fileCtrl
--  let invStat = if (invS == "flipSpin") then (-1) else 1
  let invStat = 1
  putStrLn $ show spinnya
  putStrLn $ show legend
  -- aos :: Ni:Ni#3d:6:7:9:8:10-Co:Co#3d:6:7:8:9:10-O:O#2p:3:4:5
  -- aos :: Ni:Ni#3d:d-Co:Co#3d:d-O:O#2p:p
  -- aos :: Ni:Ni#3d:d-Co:Co#3d:d-O:O#2p:p
  -- aos :: d@11+12+13+14:Ni 3d-d
  -- mirip:
  -- kudu p@7-dx2My2@11-dz2@11
  let daftaratomOs =  map (splitOn "@") $ splitOn "-" atomOs
  let theTailer = takeExtension $ T.unpack fileCtrl
  uniqAtoms <-  readUniqAtoms <$> readCtrlAtoms theTailer foldernya
    {-
  let daftarCetak'  = zip [1..]
                      $ map (\(a,label,b) -> (head $ filter (\(_,_,aa) -> aa == (T.pack a)) uniqAtoms , label, b) )
                      $ map ( (\(a:label:as) -> (a,label,as)) . splitOn ":") daftaratomOs
      daftarCetak = [ (i,j) | i <- daftarCetak' , j <- [1,2] ]
      -}
  daftarCetak <- genDaftarCetak uniqAtoms "" "" daftaratomOs
  let hasilSemua =
              map (\((_,(_,a,_)) ,p) -> insertLabel (T.unpack $ T.replace "#" " " $ T.pack a) "at graph 0.85,0.92 font 'Times New Roman Bold,10'" p)
              $ zip daftarCetak'
              $ map ((++) "plot ")
              $ map (intercalate ", ")
              $ chunksOf 2
              $ map (susunOrbs "dos" foldernya tailer invStat) daftarCetak
                {-
    return $ concat
        $ (\a -> concat  [ (init a)
                         , [ "set format x '% h';"
                           , "set xtics font 'Times New Roman,10' nomirror offset -.15,.6 out;"
                           , (last a)
                           ]
                         ]
          )
        $ map (insertText "unset label")
        $ map (insertLabel "spin-up" "at graph 0.15,0.9 font ',10'")
        $ map (insertLabel "spin-down" "at graph 0.15,0.1 font ',10'") hasilSemua
        -} {-
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
          zip  ctrlAtoms [1..nAtom]
    -- daftarCetak : [(nourut,,jumlah,nourut,symbol)]
  let daftarCetak'  = zip [1..]
                      $ map (\(a,label,b) -> (head $ filter (\(_,_,aa) -> aa == (T.pack a)) uniqAtoms , label, b) )
                      $ map ( (\(a:label:as) -> (a,label,as)) . splitOn ":") aos
      daftarCetak = [ (i,j) | i <- daftarCetak' , j <- [1,2] ]
      hasilTot'' = if hasilTot' /= "" then hasilTot' else ""
      hasilTot  = insertLabel "Energy (eV)" (concat ["at ",labelEX,",",labelEY])
                  $ insertLabel "DOS (states/eV/unit-cell)" (concat ["rotate left at ",labelDOSX,",",labelDOSY])
                  $ insertLabel jd "at graph 0.2,1.08"
                  $ insertLabel "Total" "at graph 0.85,0.92 font 'Times New Roman Bold,10'"
                  -- $ insertLabel "Total" (concat ["at ",labelXr,",",labelYr," font 'Times New Roman Bold,10'"])
                  $ (++) "plot " hasilTot''
                  -}
  {-
    let hasilSemua = hasilTot : (
              map (\((_,(_,a,_)) ,p) -> insertLabel (T.unpack $ T.replace "#" " " $ T.pack a) "at graph 0.85,0.92 font 'Times New Roman Bold,10'" p)
              $ zip daftarCetak'
              $ map ((++) "plot ")
              $ map (intercalate ", ")
              $ chunksOf 2
              $ map (susunOrbs "dos" foldernya tailer invStat) daftarCetak
              )
    return $ concat
        $ (\a -> concat  [ (init a)
                         , [ "set format x '% h';"
                           , "set xtics font 'Times New Roman,10' nomirror offset -.15,.6 out;"
                           , (last a)
                           ]
                         ]
          )
        $ map (insertText "unset label")
        $ map (insertLabel "spin-up" "at graph 0.15,0.9 font ',10'")
        $ map (insertLabel "spin-down" "at graph 0.15,0.1 font ',10'") hasilSemua
        -}
  plotPDOS useOldBw atomOs sisa (colorId+1) (iniSetting,(resSpin1,resSpin2):res)



--plotStatementDOS _ = return "====Error: plotStatementDOS @CPVO/IO/Plot/Gnuplot/DOS.hs:30"

plotTDOS :: Bool
               -> String
               -> [String]
               -> Int
               -> (PlotSetting , [(T.Text, T.Text)])
               -> IO (PlotSetting ,[(T.Text, T.Text)])
plotTDOS  _        _      []                   _       res = return res
--plotStatementDOS (jd:xr:ymax':wTot:_:invS:tailer:foldernya:aos) = do
plotTDOS  useOldBw atomOs (daftarLengkap:sisa) colorId (iniSetting,res) = do
  putStrLn "====plotTDOS======="
  let (foldernya:spinnya:legend:_) = splitOn ":" daftarLengkap
  fileCtrl <- fmap head $ inshell2text $ "ls " ++ foldernya ++ "/ctrl.*"
  putStrLn $ "====" ++ show fileCtrl ++ "======" ++ show foldernya
--  fCtrl <- T.readFile $ T.unpack fileCtrl
--  let invStat = if (invS == "flipSpin") then (-1) else 1
  let invStat = 1
  putStrLn $ show spinnya
  putStrLn $ show legend
  -- aos :: Ni:Ni#3d:6:7:9:8:10-Co:Co#3d:6:7:8:9:10-O:O#2p:3:4:5
  --let daftaratomOs =  map (splitOn "@") $ splitOn "-" atomNos
  let theTailer = takeExtension $ T.unpack fileCtrl
    {-
  let aos = splitOn "-" atomOs
  let xr = xrange iniSetting
      [xmin,xmax] = map (read :: String -> Double) $ splitOn ":" xr
  let ymax' = last $ splitOn ":" $ yrange iniSetting
  let ymax = read ymax' :: Double

      labelEX = show $ ((xmax + xmin)*0.5) - 2.5
      labelEY = show $ foldr (*) 1 [ (-1), ymax, (*) 2.5 $ fromIntegral $ length aos]
      labelDOSX = show $ xmin - 2.25
      labelDOSY = show $ foldr (*) 1 [ (-1), ymax, (+) 1 $ fromIntegral $ length aos]
-}
  putStrLn $ "=1===" ++ foldernya
  let [resSpin1,resSpin2] = map T.pack $ map (susunTot foldernya theTailer invStat ) ([1,2] :: [Integer])
  putStrLn $ "=2====" ++ T.unpack fileCtrl
    {-
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
          zip  ctrlAtoms [1..nAtom]
    -- daftarCetak : [(nourut,,jumlah,nourut,symbol)]
  let daftarCetak'  = zip [1..]
                      $ map (\(a,label,b) -> (head $ filter (\(_,_,aa) -> aa == (T.pack a)) uniqAtoms , label, b) )
                      $ map ( (\(a:label:as) -> (a,label,as)) . splitOn ":") aos
      daftarCetak = [ (i,j) | i <- daftarCetak' , j <- [1,2] ]
      hasilTot'' = if hasilTot' /= "" then hasilTot' else ""
      hasilTot  = insertLabel "Energy (eV)" (concat ["at ",labelEX,",",labelEY])
                  $ insertLabel "DOS (states/eV/unit-cell)" (concat ["rotate left at ",labelDOSX,",",labelDOSY])
                  $ insertLabel jd "at graph 0.2,1.08"
                  $ insertLabel "Total" "at graph 0.85,0.92 font 'Times New Roman Bold,10'"
                  -- $ insertLabel "Total" (concat ["at ",labelXr,",",labelYr," font 'Times New Roman Bold,10'"])
                  $ (++) "plot " hasilTot''
                  -}
  plotTDOS useOldBw atomOs sisa (colorId+1) (iniSetting,(resSpin1,resSpin2):res)


  {-
    let hasilSemua = hasilTot : (
              map (\((_,(_,a,_)) ,p) -> insertLabel (T.unpack $ T.replace "#" " " $ T.pack a) "at graph 0.85,0.92 font 'Times New Roman Bold,10'" p)
              $ zip daftarCetak'
              $ map ((++) "plot ")
              $ map (intercalate ", ")
              $ chunksOf 2
              $ map (susunOrbs "dos" foldernya tailer invStat) daftarCetak
              )
    return $ concat
        $ (\a -> concat  [ (init a)
                         , [ "set format x '% h';"
                           , "set xtics font 'Times New Roman,10' nomirror offset -.15,.6 out;"
                           , (last a)
                           ]
                         ]
          )
        $ map (insertText "unset label")
        $ map (insertLabel "spin-up" "at graph 0.15,0.9 font ',10'")
        $ map (insertLabel "spin-down" "at graph 0.15,0.1 font ',10'") hasilSemua
        -}
--plotStatementDOS _ = return "====Error: plotStatementDOS @CPVO/IO/Plot/Gnuplot/DOS.hs:30"


plotStatementDOS :: [String] -> IO String
--plotStatementDOS (jd:xr:ymax':wTot:tumpuk:invS:tailer:foldernya:aos) = do
plotStatementDOS (jd:xr:ymax':wTot:_:invS:tailer':foldernya:aos) = do
    fCtrl <- T.readFile $ foldernya ++ "/ctrl." ++ tailer'
    let invStat = if (invS == "flipSpin") then (-1) else 1
    let ymax = read ymax' :: Double
        [xmin,xmax] = map (read :: String -> Double) $ splitOn ":" xr
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
          zip  ctrlAtoms [1..nAtom]
    -- daftarCetak : [(nourut,,jumlah,nourut,symbol)]
    let daftarCetak'  = zip [1..]
                      $ map (\(a,label,b) -> (head $ filter (\(_,_,aa) -> aa == (T.pack a)) uniqAtoms , label, b) )
                      $ map ( (\(a:label:as) -> (a,label,as)) . splitOn ":") aos
        daftarCetak = [ (i,j) | i <- daftarCetak' , j <- [1,2] ]

        labelEX = show $ ((xmax + xmin)*0.5) - 2.5
        labelEY = show $ foldr (*) 1 [ (-1), ymax, (*) 2.5 $ fromIntegral $ length aos]
        labelDOSX = show $ xmin - 2.25
        labelDOSY = show $ foldr (*) 1 [ (-1), ymax, (+) 1 $ fromIntegral $ length aos]

    let hasilTot' = if (wTot == "T") then intercalate "," $ map (susunTot foldernya tailer' invStat ) ([1,2] :: [Integer]) else ""
        hasilTot'' = if hasilTot' /= "" then hasilTot' else ""
        hasilTot  = insertLabel "Energy (eV)" (concat ["at ",labelEX,",",labelEY])
                  $ insertLabel "DOS (states/eV/unit-cell)" (concat ["rotate left at ",labelDOSX,",",labelDOSY])
                  $ insertLabel jd "at graph 0.2,1.08"
                  $ insertLabel "Total" "at graph 0.85,0.92 font 'Times New Roman Bold,10'"
                  -- $ insertLabel "Total" (concat ["at ",labelXr,",",labelYr," font 'Times New Roman Bold,10'"])
                  $ (++) "plot " hasilTot''

    let hasilSemua = hasilTot : (
              map (\((_,(_,a,_)) ,p) -> insertLabel (T.unpack $ T.replace "#" " " $ T.pack a) "at graph 0.85,0.92 font 'Times New Roman Bold,10'" p)
              $ zip daftarCetak'
              $ map ((++) "plot ")
              $ map (intercalate ", ")
              $ chunksOf 2
              $ map (susunOrbs "dos" foldernya tailer' invStat) daftarCetak
              )
    return $ concat
        $ (\a -> concat  [ (init a)
                         , [ "set format x '% h';"
                           , "set xtics font 'Times New Roman,10' nomirror offset -.15,.6 out;"
                           , (last a)
                           ]
                         ]
          )
        $ map (insertText "unset label")
        $ map (insertLabel "spin-up" "at graph 0.15,0.9 font ',10'")
        $ map (insertLabel "spin-down" "at graph 0.15,0.1 font ',10'") hasilSemua
plotStatementDOS _ = return "====Error: plotStatementDOS @CPVO/IO/Plot/Gnuplot/DOS.hs:30"

pSubPlot :: ((a0, (a1, String, c0)), String) -> String
pSubPlot ( (_,(_,a,_)) ,p) = unlines [
    concat [ "set label '",a,"' at 3,15 font 'Times New Roman Bold,10'"]
  , unwords ["plot", p]
  ]

insertText :: String -> String -> String
insertText t p = unlines [t,p]

insertLabel :: String -> String -> String -> String
insertLabel l a p = unlines [ concat [ "set label '",l,"' ",a], p]

susunOrbs :: T.Text
                -> String
                -> String
                -> Int
                -> ((Int,((Int, Int, T.Text),String,[String])),Int) -- DaftarCetak
                -> String
--susunOrbs job foldernya tailer invStat ((urutan,((jumlah,nomor,nama),judul,listOrbital)),spin) = unwords [
susunOrbs job foldernya tailer' invStat ((urutan,((jumlah,nomor,_),_,listOrbital)),spin) = unwords [
                                        Text.Printf.printf "'%s/%s.isp%d.site%03d.%s' u ($1*rydberg):((%s)*%d*(%d)*(%d)/rydberg ) w l ls %d notitle"
                                          (T.pack foldernya)
                                          job
                                          spin
                                          nomor
                                          (T.pack tailer')
                                          ("$" ++ (intercalate "+$" $ delta (listOrbital /= []) listOrbital $ map show ([2..26] :: [Integer])))
                                          jumlah
                                          invStat
                                          (delta (spin < 2) 1 (-1) :: Int)
                                          urutan
                                          ]


susunTot :: String -> String -> Int -> Integer -> String
susunTot foldernya tailer' invStat spin = unwords [
                                        Text.Printf.printf "'%s/dos.tot%s' u ($1*rydberg):($%d *( %d ) *( %d ) / rydberg ) w l lc rgb 'black' notitle"
                                          (T.pack foldernya)
                                          (T.pack tailer')
                                          (delta (spin < 2) 2 (3) :: Int)
                                          invStat
                                          (delta (spin < 2) 1 (-1) :: Int)
                                          ]


