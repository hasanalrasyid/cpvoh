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
import Text.Printf (printf)
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
  tempDir <- (T.unpack . head) <$> inshell2text "mktemp -d -p ./"
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
  generatedPBAND <- genPBAND useOldBw spinnya "0" atomOs [daftarFolder]
  putStrLn $ show generatedPBAND
  let plotplate2 = T.unpack $ T.unwords [plotplate, ",", generatedPBAND]
  putStrLn $ show plotplate2
  putStrLn $ "============bikin TEMPGLT"
  tempGLT <- head <$> inshell2text "mktemp -p ./"
  putStrLn $ show tempGLT
  (xrangeatas,ticksbaru) <- genBandTicks foldernya
  putStrLn $ "========" ++ show xrangeatas ++ "=====" ++ show ticksbaru
  writeFile (T.unpack tempGLT) $ genTEMPGLT tempDir judulUtama yr xrangeatas ticksbaru (fromJust arrow) isi plotplate2 ender
    {-
  putStrLn $ show plotplate
  putStrLn $ show batasAtasX
  putStrLn $ show daftarFolder
  putStrLn $ show inp
-}
  putStrLn "===end  : plotBand===="

plotBand _ = putStrLn "Error: complete arguments needed"

genPBAND oldBw spin invStat atomNos foldernya = do
    let daftaratomOs =  map (splitOn "@") $ splitOn "-" atomNos
        daftarJudulSpinFolders = filter (/=[""]) $ map (splitOn "@") $ splitOn ":" $ unwords foldernya
        folder = last $ last daftarJudulSpinFolders
        daftarAOJSF = zip [1..] $ concat $ map (\a -> zip daftaratomOs $ repeat a) daftarJudulSpinFolders

    let err'' = map ( \(_,([o,a],[j,s,folder])) -> concat [ "cd ", folder, "; echo BandWeight.py PROCAR.",cekSpin s invStat "1" "UP" "DN" , " ",a, " ", o]) daftarAOJSF
    theErr <- if (oldBw /= "1") then mapM inshell2text err'' else return []

{--
  (\$2>6.08991818?0.4+\$2:\$2):(\$2>6.08991818&&\$2<6.09991818?1/0:\$3)
--}
    return $ T.pack $ concat $ intercalate [", "]
        $ map ( \(i,([o,a],[j,s,folder])) -> ["'",folder,"/bw.",a,".",o,".PROCAR.",cekSpin s invStat "1" "UP" "DN" ,".dat' u ($1>6.09971818?0.4+$1:$1):2:($1>6.08991818&&$1<6.09991818?0:$3*3) ls ",show (i)," ps variable title '",concat $ intersperse "." [o,a,j],"'"] :: [String] )
        $ daftarAOJSF


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


cekSpin s "0" ok sOk sNo = if s == ok then sOk else sNo
cekSpin s _ ok sOk sNo = if s == ok then sNo else sOk




susun tailer spin (urutan, [legend, foldernya]) =
    concat  [ "'", foldernya, "/dos.tot.", tailer, "'"
            , " u ($",show spin , "/rydberg):($1*rydberg)   w l ls ", show urutan
            , " title '", legend, "'"
            ]

susunGap :: (Integer, [String]) -> String
susunGap (urutan, [bandgap,condBandY]) = concat [ "set label sprintf ('{/Symbol D}=%.2feV',"
                                     , bandgap
                                     , ") at 4+", show urutan ,",(",condBandY,"-0.4) font ',12' ;"
                                     , "set arrow from ",show urutan ,",", condBandY, " to ",show urutan ,",", condBandY, "-", bandgap, " heads noborder lw 2 lc rgb 'blue'"
                                     ]
susunGap _ = "#no bandgap"


genBandTicks foldernya = do
    lTicks'' <- inshell2text $ unwords ["tail -n2  ", foldernya ++ "/bnd*spin1|sed -e '/^ $\\|==/d'|sort -u|awk '{print $2}' "]
    namaTicks'' <- inshell2text $ unwords ["cat ",foldernya ++ "/syml.* "]
    let lTicks = (:) "0.0" $ map T.unpack $ filter (not . T.null) lTicks''
    let namaTicks =  map (\a -> if a == "Gamma" then "{/Symbol G}" else a) $ (map head $ filter (/=[]) $ map (snd . (splitAt 7) . words . T.unpack)  namaTicks'')
    --putStrLn $ show $ (\a -> zip a $ ("0.0" ++ filter (/="") $ map T.unpack lTicks)) namaTicks
    return $ (,) (last lTicks)
           $ unwords $ intersperse "," $ map (\(a,b) -> unwords ["'"++a++"'",b]) $ zip  namaTicks lTicks



gantiSpin sp ((urutan,((jumlah,nomor,nama),judul,listOrbital)),_) = ((urutan,((jumlah,nomor,nama),judul,listOrbital)),(read sp :: Int))
pilihOrbs "py" = [3]
pilihOrbs "pz" = [4]
pilihOrbs "px" = [5]
pilihOrbs "dxy" = [6]
pilihOrbs "dyz" = [7]
pilihOrbs "dz2" = [8]
pilihOrbs "dxz" = [9]
pilihOrbs "dx2My2" = [10]
pilihOrbs "eg" = [8,10]
pilihOrbs "t2g" = [6,7,9]
pilihOrbs "p" = [3,4,5]
pilihOrbs _ = [6,7,8,9,10]


delta :: Bool -> b -> b -> b
delta x y z = if x then y else z

susunOrbs :: T.Text
            -> ((Int,((Int, Int, T.Text),String,[String])),Int)
            -> String
            -> String
            -> Int
            -> String
susunOrbs job ((urutan,((jumlah,nomor,nama),judul,listOrbital)),spin) foldernya tailer invStat = Text.Printf.printf "'%s/%s.isp%d.site%03d.%s' u (( %s ) * %d * ( %d ) * ( %d ) / rydberg ):($1*rydberg) w l ls %d title '%s'"
                    (T.pack foldernya)
                    job
                    spin
                    nomor
                    (T.pack tailer)
                    ("$" ++ (intercalate "+$" $ delta (listOrbital /= []) listOrbital $ map show [2..26] ))
                    jumlah
                    (if (invStat == 0) then 1 else (-1) :: Int)
--                    (delta (spin < 2) 1 (-1) :: Int)
                    ( 1 :: Int)
                    urutan
                    (unwords [judul,"a",show nomor,"s", show spin,foldernya])

  {-
mainalt tumpuk invS tailer foldernya aos  = do
--    (tumpuk:invS:tailer:foldernya:aos) <- getArgs
    fCtrl <- T.readFile $ foldernya ++ "/ctrl." ++ tailer
    let invStat = read invS :: Int
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
    let uniqAtoms =
          map (\a -> (length a, snd $ head a, fst $ head a)) $
          groupBy (\a b -> fst a == fst b) $
          zip  ctrlAtoms [1..nAtom]
    let daftarCetak' =
          zip [1..] $
          map (\(a,label,b) -> (head $ filter (\(_,_,aa) -> aa == (T.pack a)) uniqAtoms , label, b) ) $
          map ( (\(a:label:as) -> (a,label,as)) . splitOn ":") $
          aos
        daftarCetak = [ (i,j) | i <- daftarCetak' , j <- [1,2] ]
    putStrLn $ if tumpuk == "p" then  (", " ++) $ intercalate ", " $ map (\dc -> susunPDOS "dos" dc  foldernya tailer invStat) daftarCetak
                                else unlines $  (\a ->  concat [ (init a)
                                                              , [ "set format x '% h';"
                                                                , "set xtics 1 font 'Arial Bold,10' nomirror offset -.3,.6 out;"
                                                                , (last a)
                                                                ]
                                                               ]
                                                       ) $
                                                (\(x:xs) -> (", " ++ x):map ("plot " ++) xs) $
                                                map (intercalate ", ") $ chunksOf 2 $ map (\dc -> susunOrbs "dos" dc  foldernya tailer invStat) daftarCetak


susunPDOS :: T.Text
            -> ((Int,((Int, Int, T.Text),String,[String])),Int)
            -> String
            -> String
            -> Int
            -> String
susunPDOS job ((urutan,((jumlah,nomor,nama),judul,listOrbital)),spin) foldernya tailer invStat = unwords [
                                        Text.Printf.printf "'%s/%s.isp%d.site%03d.%s' u ($1*rydberg):(( %s ) * %d * ( %d ) * ( %d ) / rydberg ) w l ls %d title '%s'"
                                          (T.pack foldernya)
                                          job
                                          spin
                                          nomor
                                          (T.pack tailer)
                                          ("$" ++ (intercalate "+$" $ delta (listOrbital /= []) listOrbital $ map show [2..26] ))
                                          jumlah
                                          invStat
                                          (delta (spin < 2) 1 (-1) :: Int)
                                          urutan
                                          judul]





{-
    let mmoms1 = map (\a -> inshell (T.concat ["tail -49 ",a,"|grep mmom|sed -e 's/^c//g'|awk '{print $1,$2}'|sed -e 's/mmom.//g' -e 's/ehf.//g'| tr '\n' ' '"]) empty) fLLMFs
    mmoms2 <- mapM shell2list mmoms1
    let mmoms = chunksOf (2+nAtom) $ map fst $ rights $ map T.double $ concat $ map T.words $ concat mmoms2
    putStrLn $  T.unpack $ T.unwords $ map ( T.justifyRight 5 ' ' ) $ concat [["Steps"], ctrlAtoms , ["total" , "EnergyEHF"]]
    putStrLn $ unlines $ zipWith (\a b -> T.unpack $ T.unwords [T.justifyRight 5 ' ' a,b]) llmfs $ map showMmom mmoms
    where
      showMmom :: [Double] -> Text
      showMmom aa = T.unwords $  concat [ map (T.justifyRight 5 ' ' . T.pack . Text.Printf.printf "%.2f")  $ init aa
                                  , [T.pack $ show $ last aa] ]


-}


-}

genTEMPGLT tempDir judulUtama yr xrangeatas ticksbaru arrow isi plotplate ender =
             unlines [ "#!/home/aku/bin/gnuplot -persist"
                     , "reset"
                     , "set term post landscape enhanced color 'Times-Roman' 12"
                     , "set output '" ++ tempDir ++ "/hasil.eps'"
                     , "rydberg=13.605"
                     , "if (!exists('MP_LEFT'))   MP_LEFT = .1"
                     , "if (!exists('MP_RIGHT'))  MP_RIGHT = .95"
                     , "if (!exists('MP_BOTTOM')) MP_BOTTOM = .1"
                     , "if (!exists('MP_TOP'))    MP_TOP = .9"
                     , "if (!exists('MP_GAP'))    MP_GAP = 0.05"
                     , "set multiplot layout 1,2 \\ "
                     , "  margins screen MP_LEFT, MP_RIGHT, MP_BOTTOM, MP_TOP spacing screen MP_GAP"
                     , "set xzeroaxis"
                     , "set grid"
                     , "set key right top Left"
                     , "set key samplen 1 spacing 1"
                     , "set size ratio 1.5"
                     , "set mytics 10"
                     , "set ylabel 'Energy-E_F (eV)'"
                     , "set title '" ++ judulUtama ++ "'"
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
                     , unwords [arrow, isi,plotplate]
                     , ender
                     ]
