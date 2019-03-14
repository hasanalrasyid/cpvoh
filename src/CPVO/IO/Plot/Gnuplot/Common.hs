{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPVO.IO.Plot.Gnuplot.Common
  where

-- import Turtle                       --
-- import qualified Control.Foldl as Fold
-- import System.Environment (getArgs)
import qualified Data.Text as T
-- import qualified Data.Text.IO as T
-- import qualified Data.Text.Read as T
-- import qualified Data.Text.Format as T
import Text.Printf
import Data.List.Split (splitOn)
import Data.List
-- import Data.Maybe
-- import Data.Either

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

