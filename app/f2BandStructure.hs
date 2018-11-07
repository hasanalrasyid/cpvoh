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
import Data.Either -- rights
import Numeric.LinearAlgebra.Data -- toLists, fromLists
import Data.Maybe -- fromJust
import Data.String  -- IsString

main :: IO ()
main = do
  allArgs <- getArgs

  plotBand allArgs
  putStrLn "========beres======="

plotBand :: [String] -> IO ()
plotBand (fOut:useOldBw:judulUtama:yr:atomOs:daftarLengkap) = do
  putStrLn $ unlines [ "===start: plotBand===="
                     , "run it using:"
                     , "~/kanazawa/dev/cpvoh/f2 outputfile useOldBw judulUtama yRange atomOs target1 target2 target3"
                     , "outputfile: any filename, for each filename f, we will have f.jpg and f.eps"
                     , "useOldBw  : 1 = will use old BandWidth file"
                     , "            0 = will make a new BandWidth file"
                     , "judulUtama: unimplemented yet"
                     , "yRange    : y range min:max, ex. -9.5:2"
                     , "atomOs    : atomic orbital yang ditampilkan sebagai fat band"
                     , "            format: orbital@atomNumber-orbital@atomNumber2-..."
                     , "              atomNumber: 1,2,..."
                     , "              orbital: s p px py pz dx2My2 dz2 dyz dxy dxz eg t2g"
                     , "target    : in format of folder:spin:subTitle"
                     , "            spin: 1 = UP and 2 = DOWN or inversed by invStat"
                     , "~/kanazawa/dev/cpvoh/f2 outputfile 0 '\"\"' -9.5:2 \"p@7-dx2My2@11-dz2@11\" nico2o4.invB.11G20:1:\"11G20.Majority\""
                     ]
  tempDir <- (T.unpack . head) <$> inshell2text "mktemp -d -p ./"
  let ender = unlines [ "unset multiplot"
                      , "system \"cd " ++ tempDir ++ " && epstopdf hasil.eps && pdftocairo -r 150 -singlefile -jpeg hasil.pdf tmp && convert tmp.jpg -rotate 90 hasil.jpg && rm -f tmp.jpg\""
                      ]
  let isi = unlines [ "set datafile missing '-'"
                    , "plot 0 lt -1 lc rgb 'black' title '' , \\"
                    ]
  putStrLn ender

  putStrLn $ unlines daftarLengkap

  allPBAND@((xrangeatas,ticksbaru,arrow,_,_):_) <- mapM (plotSingleBand useOldBw atomOs) daftarLengkap
  let generatedPFBAND = (map takePFBAND allPBAND)
  let generatedPBAND = T.intercalate "," (map fst generatedPFBAND)
  let generatedFATBAND = T.intercalate "," (map snd generatedPFBAND)
  let plotplate2 = T.unpack $ T.unwords [generatedPBAND, ",", generatedFATBAND]
  putStrLn $ show plotplate2
  putStrLn $ show allPBAND
  putStrLn $ "============bikin TEMPGLT"
  tempGLT <- head <$> inshell2text "mktemp -p ./"
  putStrLn $ show tempGLT
  writeFile (T.unpack tempGLT) $ genTEMPGLT tempDir judulUtama yr xrangeatas ticksbaru (fromMaybe "" arrow) isi plotplate2 ender
  let converter = unwords ["convert", tempDir ++ "/hasil.jpg"
                          , "-fuzz 5% -trim +repage"
                          , tempDir ++ "/hasil.jpg"
                          ]
  let target = map (\x -> unwords [ "mv"
                                  , tempDir ++ "/hasil" ++ x
                                  , fOut ++ x
                                  ]) [".eps",".jpg"]
  _ <- mapM inshell2text $ (("gnuplot " ++ T.unpack tempGLT) : converter : target )
  putStrLn "===end  : plotBand===="
  putStrLn plotplate2
    where
      takePFBAND (_,_,_,p,f) = (p,f)

plotBand _ = putStrLn "Error: complete arguments needed"

plotSingleBand :: String -> String -> String
               -> IO (String, String, Maybe String, T.Text, T.Text)
plotSingleBand useOldBw atomOs daftarLengkap = do
  let (foldernya:spinnya:legend:_) = splitOn ":" daftarLengkap
  bandFiles <- inshell2text $ unwords ["ls",concat[foldernya,"/bnd*spin",spinnya]]
  let daftarFolder = ":" ++ intercalate "@" [legend,spinnya,foldernya]

  let generatedPBAND = T.intercalate ", " $ map genPlotPlate $ map (\x -> T.concat["'",x,"'"]) bandFiles
  valBand@(valBandX:valBandY:_) <- fmap (words . T.unpack . head) $ inshell2text $ unwords ["cat",concat [foldernya,"/bnd*spin*",spinnya],"|sed -e '/^#/d' |awk '{if ($3>0.1) print $2,$3}'|sort -k 2n|head -1"]
  (_:condBandY:_) <- fmap (words . T.unpack . head) $ inshell2text $ unwords ["cat", concat[foldernya,"/bnd*spin*",spinnya],"| sed -e '/^#/d' |awk '{if ($3<=0) print $2,$3}'|sort -k 2nr -u|sed -e '/^ *$/d'|head -1"]
  putStrLn $ "===valBand==" ++ show (valBand :: [String])
  gapCoordBandGap <- fmap runGAPband $ inshell2text $ "cat " ++ concat [foldernya,"/bnd*spin*",spinnya]
  let bandGap = last $ words gapCoordBandGap
  putStrLn $ "===allBand=== " ++ bandGap
  let arrow =
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
--  putStrLn $ show gapArrow
--  putStrLn $ "perintahDOS Removed == " ++ unwords [
--    "genPDOSvert.hs ", atomOs, fromJust gapnya, spinnya, daftarFolder]
  generatedFATBAND <- genPBAND useOldBw spinnya "0" atomOs [daftarFolder]
  putStrLn $ show generatedFATBAND
  (xrangeatas,ticksbaru) <- genBandTicks foldernya
  putStrLn $ "========" ++ show xrangeatas ++ "=====" ++ show ticksbaru
  putStrLn $ "=== FINISHED PROCESSING : " ++ daftarLengkap
  return $ (xrangeatas,ticksbaru,arrow,generatedPBAND,generatedFATBAND)


genPBAND :: String -> String -> String -> String -> [String] -> IO T.Text
--genPBAND oldBw spin invStat atomNos foldernya = do
genPBAND   _      _   _       []      _         = return ""
genPBAND   oldBw  _   invStat atomNos foldernya = do
    let daftaratomOs =  map (splitOn "@") $ splitOn "-" atomNos
        daftarJudulSpinFolders = filter (/=[""]) $ map (splitOn "@") $ splitOn ":" $ unwords foldernya
        daftarAOJSF = zip ([1..] :: [Integer]) $ concat $ map (\a -> zip daftaratomOs $ repeat a) daftarJudulSpinFolders

    let err'' = map ( \(_,([o,a],[_,s,folder'])) -> concat [ "cd ", folder', "; BandWeight.py PROCAR.",cekSpin s invStat "1" "UP" "DN" , " ",a, " ", o]) daftarAOJSF
    _ <- if (oldBw /= "1") then mapM inshell2text err'' else return []

{--
  (\$2>6.08991818?0.4+\$2:\$2):(\$2>6.08991818&&\$2<6.09991818?1/0:\$3)
--}
    return $ T.pack $ concat $ intercalate [", "]
      $ map ( \(i,([o,a],[j,s,folder])) -> ["'",folder,"/bw.",a,".",o,".PROCAR.",cekSpin s invStat "1" "UP" "DN" ,".dat' u ($1>6.09971818?0.4+$1:$1):2:($1>6.08991818&&$1<6.09991818?0:$3*3) ls ",show i," ps variable title '",concat $ intersperse "." [o,a,j],"'"] :: [String] )
      $ daftarAOJSF

genPlotPlate :: T.Text -> T.Text
genPlotPlate bnd = T.concat [ bnd , T.pack " u ($2>6.08991818?0.4+$2:$2):($2>6.08991818&&$2<6.09991818?1/0:$3) with line lc rgb 'black' title ''"]

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

genTEMPGLT :: String -> String -> String -> String -> String -> String -> String -> String -> String -> String
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
                     , unwords [ "set multiplot layout 1,2"
                               , "margins screen MP_LEFT, MP_RIGHT, MP_BOTTOM, MP_TOP"
                               , "spacing screen MP_GAP"
                               ]
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
                     , unwords [arrow, isi ,plotplate]
                     , ender
                     ]
