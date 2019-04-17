#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

import qualified Control.Foldl as Fold
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified Data.Text.Format as T
import Text.Printf
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Either


import CPVO.IO


delta :: Bool -> a -> a -> a
delta i j k = if i then j else k

caller :: T.Text
caller = T.unlines  [ "callme with : genPDOSAtomicOrbital.hs [Atom:Orbital]                tailer    folder"
                    , "ex.         : genPDOSAtomicOrbital.hs 'O NiTd:2:3:4:5 CoTd:2:3:4:5' nico2o4   nico2o4.invB.0GGA"
                    ]


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

  {-
shell2list :: MonadIO IO => Shell a -> IO [a]
shell2list xx = fold (xx) Fold.list
-}

main = do
    (foldernya:[]) <- getArgs
    lTicks' <- inshell2text $ unwords $ ["tail -n2  ", foldernya ++ "/bnd*spin1|sed -e '/^ $\\|==/d'|sort -u|awk '{print $2}' "]
    lTicks' <- inshell2text $ unwords $ ["tail -n2  ", foldernya ++ "/bnd*spin1|sed -e '/^ $\\|==/d'|sort -u|awk '{print $2}' "]
    namaTicks' <- inshell2text $ unwords $ ["cat ",foldernya ++ "/syml.* "]
    let namaTicks'' = namaTicks'
    let lTicks'' = lTicks'
    let lTicks = (:) "0.0" $ map T.unpack $ filter (/="") lTicks''
    let namaTicks =  map (\a -> if a == "Gamma" then "{/Symbol G}" else a) $ (map head $ filter (/=[]) $ map (snd . (splitAt 7) . words . T.unpack)  namaTicks'')
    --putStrLn $ show $ (\a -> zip a $ ("0.0" ++ filter (/="") $ map T.unpack lTicks)) namaTicks
    putStrLn $ last lTicks
    putStrLn $ unwords $ intersperse "," $ map (\(a,b) -> unwords ["'"++a++"'",b]) $ zip  namaTicks lTicks

{-
    let daftarAtomOs =  map (splitOn "@") $ splitOn "-" atomOrbs
    let daftarFolder = filter (/=[[]]) $ map (splitOn "@") $ splitOn ":" $ unwords daftarFolder'
    let tailernya = last $ splitOn "." $ T.unpack $ last tailer
    fCtrl <- T.readFile $ ( last $ head daftarFolder ) ++ "/ctrl." ++ tailernya
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
--    putStrLn $ show $ ctrlAtoms !! 13 -- index untuk atom di list ini dimulai dari 0
-}
{-
    let uniqAtoms =
          map (\a -> (length a, snd $ head a, fst $ head a)) $
          groupBy (\a b -> fst a == fst b) $
          zip  ctrlAtoms [1..]
-}
{-
    let gap = unlines $ map susunGap $ zip [1..] $
              map (splitOn "@") $
              splitOn ":" gapnya
    let daftarCetak = map  (\(ls, [o,a]) -> ((ls,((1, (read a :: Int), T.pack "Text"),o , map show $ pilihOrbs o)),1)) $ zip [1..] daftarAtomOs
--susunOrbs job ((urutan,((jumlah,nomor,nama),judul,listOrbital)),spin) foldernya tailer invStat = unwords [

    let daftarCetak' = [((1,((1, 1, T.pack "Text"),"t2g",map show $pilihOrbs "t2g")),1)]
        invStat = 0   -- 1 jika di inverse spinnya
--    let plotter =   intercalate ", " $ map (\dc -> susunOrbs "dos" dc  (last $ head daftarFolder) tailernya invStat) daftarCetak
    let plotter =   intercalate ", " $ map (\(dc,[spnya,fnya]) -> susunOrbs "dos" (gantiSpin spnya dc)  fnya tailernya invStat) $ concat $ map (\a -> zip daftarCetak $ repeat a) $ map tail daftarFolder
    putStrLn $ unlines $ [ "unset arrow"
                         , "unset ylabel"
                         , "set format y '  '"
                         , "set xtics 5"
                         , "set xrange [0:5]"
                         , "rydberg=13.605"
                         , "set key right bottom Left samplen 1 spacing 1 "
                         , "set xlabel 'PDOS (states / eV) ' offset 0,0;"
                         , gap
                         , "plot " ++  plotter
                         ]
--    putStrLn $ show $head $ concat $ map (\a -> zip daftarCetak $ repeat a) $ map tail daftarFolder
--    putStrLn $ show $ gantiSpin "2" $ head daftarCetak'
--    putStrLn $ show $ daftarCetak
-}

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

susunOrbs :: T.Text
            -> ((Int,((Int, Int, T.Text),String,[String])),Int)
            -> String
            -> String
            -> Int
            -> String
susunOrbs job ((urutan,((jumlah,nomor,nama),judul,listOrbital)),spin) foldernya tailer invStat = unwords [
                                        Text.Printf.printf "'%s/%s.isp%d.site%03d.%s' u (( %s ) * %d * ( %d ) * ( %d ) / rydberg ):($1*rydberg) w l ls %d title '%s'"
                                          (T.pack foldernya)
                                          job
                                          spin
                                          nomor
                                          (T.pack tailer)
                                          ("$" ++ (intercalate "+$" $ delta (listOrbital /= []) listOrbital $ map show [2..26] ))
                                          jumlah
                                          (if (invStat == 0) then 1 else (-1) :: Int)
--                                          (delta (spin < 2) 1 (-1) :: Int)
                                          ( 1 :: Int)
                                          urutan
                                          (unwords [judul,"a",show nomor,"s", show spin,foldernya])
                                        ]
--                                          (delta (spin < 2) (unwords $ [judul,foldernya]) "")]
{-
-}
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
--                                else unlines $ ([""] ++) $map ("plot " ++) $ map (intercalate ", ") $ chunksOf 2 $ map (\dc -> susunPDOS "dos" dc  foldernya tailer invStat) daftarCetak
                                else unlines $  (\a ->  concat [ (init a)
                                                              , [ "set format x '% h';"
                                                                , "set xtics 1 font 'Arial Bold,10' nomirror offset -.3,.6 out;"
--                                                                , "set xlabel 'Energy (eV)' offset 0,1;"
--                                                                , "set ylabel 'DOS (states/eV)' offset screen 1.8," ++ (show $length a)
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



