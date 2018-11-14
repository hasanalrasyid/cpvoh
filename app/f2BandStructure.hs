{-
#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

--import CPVO.IO.Plot.Band

import CPVO.IO -- inshell2text
--import System.Environment (getArgs)
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.List -- intercalate
import Data.Either -- rights
import Numeric.LinearAlgebra.Data -- toLists, fromLists
import Data.Maybe -- fromJust
import Data.String  -- IsString
import System.Directory
import Options.Applicative as O
import Data.Semigroup -- <>

main :: IO ()
main = do
  (Opts fOut useOldBw judulUtama yr atomOs daftarLengkap) <- execParser withHelp
  plotBand fOut useOldBw judulUtama yr atomOs
    $ map (splitOn "#") daftarLengkap
  putStrLn "========DONE======="

debugLine :: Show a => Bool -> a -> IO ()
debugLine False _ = return ()
debugLine _ ss = putStrLn $ "===debug=== " ++ show ss

plotInit :: String
plotInit = unlines $ "set datafile missing '-'":
                     "plot 0 lt -1 lc rgb 'black' title '' , \\":[]

endMultiplot :: String
endMultiplot = unlines [ "unset multiplot" ]

plotBand :: String -> Bool -> String -> String -> String -> [[String]]
         -> IO ()
plotBand fOut useOldBw judulUtama yr atomOs daftarLengkap = do
  tempDir <- (T.unpack . head) <$> inshell2text "mktemp -d -p ./"

  putStrLn $ unlines $ concat daftarLengkap
  (xrangeatas,ticksbaru,arrow,plotplate) <- genSinglePic useOldBw atomOs daftarLengkap ("","",Nothing,[])

  putStrLn $ "============TEMPGLT"
  tempGLT <- head <$> inshell2text "mktemp -p ./"
  putStrLn $ show tempGLT

  writeFile (T.unpack tempGLT) $ genTEMPGLT tempDir judulUtama yr xrangeatas ticksbaru (fromMaybe "" arrow) plotplate

  system_ $ "gnuplot " ++ T.unpack tempGLT
  let target = map (\x -> unwords [ "mv "
                                  , "hasil" ++ x
                                  , "../" ++ fOut ++ x
                                  ]) [".eps",".png"]
  withCurrentDirectory tempDir $
    mapM_ system_ $
      "ps2eps --rotate=+ tmp.ps":
      "epstool --copy -b --quiet tmp.eps hasil.eps":
      "epstopdf hasil.eps":
      "pdftocairo -r 150 -singlefile -jpeg hasil.pdf tmp":
      "convert tmp.jpg -rotate 0 hasil.png":
      "rm -f tmp.jpg":
      target
  putStrLn "===end  : plotBand===="

genSinglePic :: Bool -> String -> [[String]]
              ->    (String, String, Maybe String, [String])
              -> IO (String, String, Maybe String, [String])
genSinglePic _        _      []            res              = return res
genSinglePic useOldBw atomOs (daftarLengkap:ds) (_,_,_,res) = do
  allPBAND@((xrangeatas,ticksbaru,arrow,_,_):_) <- plotSinglePic daftarLengkap useOldBw atomOs 1 []
  let generatedPFBAND = (map takePFBAND allPBAND)
  let generatedPBAND = T.intercalate "," $ filter (not . T.null) $ map fst generatedPFBAND
  let generatedFATBAND = T.intercalate "," $ filter (not . T.null) $ map snd generatedPFBAND
  genSinglePic useOldBw atomOs ds (xrangeatas, ticksbaru, arrow, (T.unpack $ T.intercalate "," $ filter (not . T.null) [generatedPBAND, generatedFATBAND]):res)
    where
      takePFBAND (_,_,_,p,f) = (p,f)

plotSinglePic :: [String]
               -> Bool
               -> String
               -> Int
               -> [(String, String, Maybe String, T.Text, T.Text)]
               -> IO [(String, String, Maybe String, T.Text, T.Text)]

plotSinglePic []                   _        _       _      res = return res
plotSinglePic (daftarLengkap:sisa) useOldBw atomOs colorId res = do
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
  let arrow = genArrow bandGap valBandX valBandY condBandY
--  putStrLn $ show gapArrow
--  putStrLn $ "perintahDOS Removed == " ++ unwords [
--    "genPDOSvert.hs ", atomOs, fromJust gapnya, spinnya, daftarFolder]
  generatedFATBAND <- genPBAND useOldBw spinnya "0" atomOs [daftarFolder]
  putStrLn $ show generatedFATBAND
  (xrangeatas,ticksbaru) <- genBandTicks foldernya
  putStrLn $ "========" ++ show xrangeatas ++ "=====" ++ show ticksbaru
  putStrLn $ "=== FINISHED PROCESSING : " ++ daftarLengkap
  plotSinglePic sisa useOldBw atomOs (colorId+1) ((xrangeatas,ticksbaru,arrow,generatedPBAND,generatedFATBAND):res)

genArrow :: [Char] -> [Char] -> [Char] -> [Char] -> Maybe String
genArrow _ _ _ _ = Nothing
genArrow' :: [Char] -> [Char] -> [Char] -> [Char] -> Maybe String
genArrow' bandGap valBandX valBandY condBandY =
        if (bandGap == "0")
           then Nothing
           else --(,) (Just $ "0:" ++ bandGap ++ "@" ++ valBandY)
                    (Just $ unlines [ concat ["set label sprintf ('{/Symbol D}=%.2feV',"
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

genPBAND :: Bool -> String -> String -> String -> [String] -> IO T.Text
--genPBAND oldBw spin invStat atomNos foldernya = do
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

genPlotPlate :: Int -> T.Text -> T.Text
genPlotPlate colorId bnd = T.concat [ bnd , T.pack $ unwords [" u ($2>6.08991818?0.4+$2:$2):($2>6.08991818&&$2<6.09991818?1/0:$3) with line lc", show colorId, "title ''"]]

runGAPband :: [T.Text] -> String
runGAPband bndSpin =
  let (valencePoint, conductionPoint) =
        (\(a,b) -> (last a,head b)) $
        break (\[_,a] -> a > 0) $
        Data.List.sortBy (\[_,a] [_,b] -> if a < b then LT else GT ) $
        toLists $
        (¿ [1,2]) . fromLists $
        filter (/=[]) $  -- kolom $2 x (k-point) -- $3 y Energy
        map ( (map fst) . rights . (map T.double) . T.words ) $
        filter (not . T.isPrefixOf "#") $
        bndSpin

  in if (last conductionPoint - last valencePoint < 0.01)  then "0 0 0"
                                                           else unwords $ map show  [head valencePoint, head conductionPoint, last conductionPoint - last valencePoint]


cekSpin :: (Eq a, IsString a) => a -> a -> a-> p -> p -> p
cekSpin s "0" ok sOk sNo = if s == ok then sOk else sNo
cekSpin s _ ok sOk sNo = if s == ok then sNo else sOk

genBandTicks :: String -> IO (String,String)
genBandTicks foldernya = do
    lTicks'' <- inshell2text $ unwords ["tail -n2  ", foldernya ++ "/bnd*spin1|sed -e '/^ $\\|==/d'|sort -u|awk '{print $2}' "]
    namaTicks'' <- inshell2text $ unwords ["cat ",foldernya ++ "/syml.* "]
    let lTicks = (:) "0.0" $ map T.unpack $ filter (not . T.null) lTicks''
    let namaTicks =  map (\a -> if a == "Gamma" then "{/Symbol G}" else a) $ (map head $ filter (/=[]) $ map (snd . (splitAt 7) . words . T.unpack)  namaTicks'')
    return $ (,) (last lTicks)
           $ unwords $ intersperse "," $ map (\(a,b) -> unwords ["'"++a++"'",b]) $ zip  namaTicks lTicks

genTEMPGLT :: String -> String -> String -> String -> String -> String
           -> [String]
           -> String
genTEMPGLT tempDir judulUtama yr xrangeatas ticksbaru arrow plotplate =
    let subTitles = splitOn "#" judulUtama
     in unlines [ "#!/home/aku/bin/gnuplot -persist"
                , "reset"
                , "set term post landscape enhanced color 'Times-Roman' 12"
                , "set output '" ++ tempDir ++ "/tmp.ps'"
                , "rydberg=13.605"
                , "if (!exists('MP_LEFT'))   MP_LEFT = .1"
                , "if (!exists('MP_RIGHT'))  MP_RIGHT = .95"
                , "if (!exists('MP_BOTTOM')) MP_BOTTOM = .1"
                , "if (!exists('MP_TOP'))    MP_TOP = .9"
                , "if (!exists('MP_GAP'))    MP_GAP = 0.05"
                , unwords [ "set multiplot layout 1," ++ (show $ length subTitles )
                          , "margins screen MP_LEFT, MP_RIGHT, MP_BOTTOM, MP_TOP"
                          , "spacing screen MP_GAP"
                          ]
                , "set xzeroaxis"
                , "set grid"
                , "set key right top Left"
                , "set key samplen 1 spacing 1"
                , "set size ratio 1.5"
                , "set mytics 10"
                , "set xlabel 'Wave Vector'"
                , "set ylabel 'Energy-E_F (eV)'"
--                , "set title '" ++ judulUtama ++ "'"
                , "set yrange ["++ yr ++ "]"
                , "set xrange [0.0:" ++ xrangeatas ++ "]"
                , "set grid noy"
                , "set grid xtics lt 0 lc rgb 'black'"
                , "set style line 1 lt 2 lw 1 lc rgb '#e41a1c'"
                , "set style line 2 lt 6 lw 1 lc rgb '#377eb8'"
                , "set style line 3 lt 6 lw 1 lc rgb '#4daf4a'"
                , "set style line 4 lt 6 lw 1 lc rgb '#984ea3'"
                , "set style line 5 lt 6 lw 1 lc rgb '#ff7f00'"
                , "set style line 8 lt 1 lw 2 lc rgb '#ffcc99'"
                , "set style line 9 lt 1 lw 2 lc rgb '#808080'"
                , "set style line 10 lt 1 lw 2 lc rgb '#94ffb5'"
                , "set style line 11 lt 1 lw 2 lc rgb '#8f7c0'"
                , "set style line 12 lt 1 lw 2 lc rgb '#9dcc0'"
                , "set style line 13 lt 1 lw 2 lc rgb '#c2088'"
                , "set style line 14 lt 1 lw 2 lc rgb '#03380'"
                , "set style line 15 lt 1 lw 2 lc rgb '#ffa45'"
                , "set style line 16 lt 1 lw 2 lc rgb '#ffa8bb'"
                , "set style line 17 lt 1 lw 2 lc rgb '#42660'"
                , "set style line 18 lt 1 lw 2 lc rgb '#ff010'"
                , "set style line 19 lt 1 lw 2 lc rgb '#5ef1f2'"
                , "set style line 20 lt 1 lw 2 lc rgb '#0998f'"
                , "set style line 21 lt 1 lw 2 lc rgb '#e0ff66'"
                , "set style line 22 lt 1 lw 2 lc rgb '#74aff'"
                , "set style line 23 lt 1 lw 2 lc rgb '#9900'"
                , "set style line 24 lt 1 lw 2 lc rgb '#ffff80'"
                , "set style line 25 lt 1 lw 2 lc rgb '#ffff0'"
                , "set style line 26 lt 1 lw 2 lc rgb '#ff505'"
                , "set style arrow 1 heads size screen 0.01,90 lw 2 lc rgb 'navy'"
                , "set key bottom left Left"
                , "set xtics (" ++ ticksbaru ++ ")"
                , (unlines
                     $ intersperse noMidLabel
                     $ zipWith (\s p -> unlines $ ("set title '" ++ s ++ "'"):
                                        unwords [arrow, plotInit ,p]:
                                        [])
                       subTitles plotplate
                  )
                , endMultiplot
                ]

noMidLabel :: String
noMidLabel = unlines $ "unset ylabel":
                       []

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
