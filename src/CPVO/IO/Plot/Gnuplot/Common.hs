{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPVO.IO.Plot.Gnuplot.Common
  where

-- import Turtle                       --
-- import qualified Control.Foldl as Fold
-- import System.Environment (getArgs)
import CPVO.IO.Plot.Gnuplot.Type
import CPVO.IO
import qualified Data.Text as T
-- import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
-- import qualified Data.Text.Format as T
import Text.Printf
import Data.List.Split (splitOn)
import Data.List
-- import Data.Maybe
-- import Data.Either

import System.Directory
import Data.Either
import Numeric.LinearAlgebra.Data
import Data.String -- IsString

warna :: [T.Text]
warna = [
    "#e41a1c"
  , "#377eb8"
  , "#4daf4a"
  , "#984ea3"
  , "#ff3399"
--  , "#006d2c"
--  , "#993f00"
--  , "#4c05c0"
--  , "#191919", "#05c310"
--  , "#2bce48", "#ffcc99"
--  , "#808080", "#94ffb5"
--  , "#8f7c00", "#9dcc00"
--  , "#c20880", "#033800"
--  , "#ffa450", "#ffa8bb"
--  , "#426600", "#ff0100"
--  , "#5ef1f2", "#0998f0"
--  , "#e0ff66", "#74aff0"
--  , "#990000", "#ffff80"
--  , "#ffff00", "#ff5050"
--    "#e41a1c"
--    , "#377eb8"
--    , "#4daf4a"
--    , "#984ea3"
--    , "#ff7f00"
--    , "#ff3399"
--    , "#2ca25f"
--    , "#006d2c"
--    , "#fdae61"
--    , "#ffffbf"
--    , "#abdda4"
--    , "#993f0"
--    , "#4c05c"
--    , "#191919"
--    , "#05c31"
--    , "#2bce48"
--    , "#ffcc99"
--    , "#808080"
--    , "#94ffb5"
--    , "#8f7c0"
--    , "#9dcc0"
--    , "#c2088"
--    , "#03380"
--    , "#ffa45"
--    , "#ffa8bb"
--    , "#42660"
--    , "#ff010"
--    , "#5ef1f2"
--    , "#0998f"
--    , "#e0ff66"
--    , "#74aff"
--    , "#9900"
--    , "#ffff80"
--    , "#ffff0"
--    , "#ff505"
    ]

-- shell2list :: MonadIO io => Shell a -> io [a]
-- shell2list xx = fold (xx) Fold.list
--
-- diracDelta f x = if ( f x == 0 ) then 1 else 0
genLineType :: String
genLineType = unlines
        $ map (\(nomor,warnanya) -> Text.Printf.printf "set style line %d lt %d lw 1 lc rgb '%s'" nomor nomor warnanya )
        $ zip [1..(length warna)] warna

genTOP :: [String] -> String
genTOP (xr:yr:poskey:_) = unlines [ "#!/home/aku/bin/gnuplot -persist"
                 , "reset"
                 , "set term post portrait enhanced color font 'Times-Roman'"
                 , "set output 'plots/tmp.ps'"

                 , "if (!exists('MP_LEFT'))   MP_LEFT = .1"
                 , "if (!exists('MP_RIGHT'))  MP_RIGHT = .91"
                 , "if (!exists('MP_BOTTOM')) MP_BOTTOM = .1"
                 , "if (!exists('MP_TOP'))    MP_TOP = .9"
                 , "if (!exists('MP_GAP'))    MP_GAP = 0.0"

                 , "set tmargin 0"
                 , "set bmargin 0"
                 , "set lmargin 8"
                 , "set rmargin 6"

                 , "set multiplot layout 5,2 columnsfirst title '{/:Bold=15   }' \\"
                 , " margins screen MP_LEFT, MP_RIGHT, MP_BOTTOM, MP_TOP spacing screen MP_GAP"

                 , "set border lw 0.2"

                 , "set key " ++ poskey ++ " font 'Times New Roman Bold,8'"
                 , "set key spacing 1"
                 , "set key samplen 0"
                        , "set xrange [" ++ xr ++ "]"
                        , "set yrange [" ++ yr ++ "]"
                 , "set mxtics 2"
                 , "set mytics 2"
                 , "rydberg=13.605"

                 , "unset grid"

                        , "#set arrow from 0,-" ++ yr ++ " to 0," ++ yr ++ " nohead lc rgb 'navy'"

                 , genLineType

                 , "f(x) = 0"

                 , "set xzeroaxis lw 1 lt 1 lc rgb 'black'"
                 , "set yzeroaxis lw 1 lt 1 lc rgb 'black'"

                 , "set style data boxes"
                 , "unset ylabel"
                 , "unset xtics"
                        , "set ytics (" ++ yticsnya ++ ") font ',10' nomirror offset .7"
                        , "set xtics " ++ xticsnya ++ " font ',10' nomirror offset 0,.6"
                 , "set format x ''"
                 , "#############################################################################################"
                 ]
                   where
                     xticsnya = "-100,2,100"
                     (yInit:yLast:_) = map read $ splitOn ":" yr :: [Int]
--                     yDelta' = (flip quot) 5 $ floor $ (fromIntegral $ yLast - (if yInit < 0 then 0 else yInit)) * (1 / 3.0 :: Double)
                     yDelta = (*5) $ round $ (flip (/)) 5 $ (fromIntegral $ yLast - (if yInit < 0 then 0 else yInit)) / 3.0
                     yticsnya' =  [ x | a <- [0..], b <- [1,(-1)], let x = a * yDelta * b ]
                     yticsnya = intercalate "," $ map show $ tail $ takeWhile (\x -> x < yLast && x > yInit) yticsnya'
genTOP _ = "Error: genTOP @CPVO/IO/Plot/Gnuplot/Common.hs:142"

genEnder :: String
genEnder = unlines [ "unset multiplot"
                   , "system 'cd plots && ps2eps tmp.ps && epstool --copy -b --quiet tmp.eps hasil.eps && epstopdf hasil.eps && pdftocairo -r 150 -singlefile -jpeg hasil.pdf tmp && convert tmp.jpg -rotate 0 hasil.png && rm -f tmp.jpg tmp.eps'"
                   ]

totalHeader :: String
totalHeader = unlines [ "###########Total Header#############"
                      , "set label 'Total' at 2.5,15 font 'Bold,8'"
                      , "set label 'spin-up' at 2.5,7.5 font ',8'"
                      , "set label 'spin-down' at 2.5,-7.5 font ',8'"
                      ]

plotInit :: String
plotInit = unlines $ "set datafile missing '-'":
                     "plot 0 lt -1 lc rgb 'black' title '' , \\":[]

endMultiplot :: String
endMultiplot = unlines [ "unset multiplot" ]

plotWork iniSetting fOut plotter daftarLengkap = do
  tempDir <- (T.unpack . head) <$> inshell2text "mktemp -d -p ./"

  putStrLn $ unlines $ concat daftarLengkap
  (plotSet,plotplate) <- genAllPics plotter daftarLengkap (iniSetting,[])
  debugIt "daftarLengkap " [daftarLengkap]
  putStrLn $ "============TEMPGLT"
  tempGLT <- head <$> inshell2text "mktemp -p ./"
  putStrLn $ show tempGLT
  writeFile (T.unpack tempGLT) $ genTEMPGLT tempDir plotSet plotplate

  system_ $ "gnuplot " ++ T.unpack tempGLT
  let target = map (\x -> unwords [ "cp -f "
                                  , "hasil" ++ x
                                  , "../" ++ fOut ++ x
                                  ]) [".eps",".png"]
  withCurrentDirectory tempDir $
    mapM_ system_ $
      "ps2eps --rotate=+ tmp.ps":
      "epstool --copy -b --quiet tmp.eps tmp0.eps":
      "epstopdf tmp0.eps hasil.pdf":
      "pdftops hasil.pdf hasil.ps":
      "ps2eps hasil.ps":
      "pdftocairo -r 150 -singlefile -jpeg hasil.pdf tmp":
      "convert tmp.jpg -rotate 0 hasil.png":
      "rm -f tmp.jpg":
      target
  putStrLn "===end  : plotWork===="

genAllPics _       []                 res     = return res
genAllPics plotter (daftarLengkap:ds) (iniSetting,res) = do
  putStrLn "====genAllPics: INIT"
  (plotSetting,generatedPFBAND) <- plotter daftarLengkap 1 (iniSetting,[])
  putStrLn "====genAllPics: plotting DONE"
  let generatedPBAND = T.intercalate "," $ filter (not . T.null) $ map fst generatedPFBAND
  let generatedFATBAND = T.intercalate "," $ filter (not . T.null) $ map snd generatedPFBAND
  putStrLn $ "===== genAllPics DONE" ++ show daftarLengkap
  genAllPics plotter ds (plotSetting, (T.unpack $ T.intercalate "," $ filter (not . T.null) [generatedPBAND, generatedFATBAND]):res)

genArrow :: [Char] -> [Char] -> [Char] -> [Char] -> String
genArrow _ _ _ _ = ""
genArrow' :: [Char] -> [Char] -> [Char] -> [Char] -> String
genArrow' bandGap valBandX valBandY condBandY =
        if (bandGap == "0")
           then ""
           else --(,) (Just $ "0:" ++ bandGap ++ "@" ++ valBandY)
                    (unlines [ concat ["set label sprintf ('{/Symbol D}=%.2feV',"
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

genTEMPGLT :: String -> PlotSetting -> [String] -> String
genTEMPGLT _       NullSetting _ = ""
genTEMPGLT tempDir (PlotSetting _ judulUtama yr xr xticks ar lbs) plotplate =
    let subTitles = splitOn "#" judulUtama
        newxticks = if (null xticks) then ""
                                     else "set xtics (" ++ xticks ++ ")"
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
                , lbs
                , "set yrange ["++ yr ++ "]"
                , "set xrange [" ++ xr ++ "]"
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
                , newxticks
                , (unlines
                     $ intersperse noMidLabel
                     $ zipWith (\s p -> unlines $ ("set title '" ++ s ++ "'"):
                                        unwords [ar, plotInit ,p]:
                                        [])
                       subTitles plotplate
                  )
                , endMultiplot
                ]

noMidLabel :: String
noMidLabel = unlines $ "unset ylabel":
                       []
