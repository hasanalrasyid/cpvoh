{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module CPVO.IO.Plot.Gnuplot.DOS
  where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Printf
import Data.List.Split
import Data.List
import Data.Maybe
import Data.List.Utils (replace)

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

plotStatementDOS :: [String] -> IO String
--plotStatementDOS (jd:xr:ymax':wTot:tumpuk:invS:tailer:foldernya:aos) = do
--plotStatementDOS (jd:xr:ymax':wTot:     _:invS:tailer:foldernya:aos) = do
plotStatementDOS   ( _:xr:ymax':   _:     _:invS:tailer:foldernya:aos) = do
    putStrLn $ "======plotStatementDOS INIT"
    fCtrl <- T.readFile $ foldernya ++ "/ctrl." ++ tailer
    let invStat = if (invS == "flipSpin") then (-1) else 1
    let (ymin:ymax:_) = map read $ splitOn ":" ymax' :: [Double]
--        [xmin,xmax] = map (read :: String -> Double) $ splitOn ":" xr
        [xmin,_] = map (read :: String -> Double) $ splitOn ":" xr
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
    putStrLn $ "===" ++ show (uniqAtoms,ctrlAtoms,aos)
    let daftarCetak = [ (i,j) | i <- daftarCetak' , j <- [1,2] ]

    let tot = map (susunTot foldernya tailer invStat ) ([1,2] :: [Integer])
        spinHead = if ymin >= 0 then ""
                            else unlines $
                              "set label 'spin-up' at graph 0.15,0.8 font ',10'":
                              "set label 'spin-down' at graph 0.15,0.2 font ',10'":
                              []
        thead = unlines $
                "set label 'Total' at graph 0.80,0.92 font 'Times New Roman Bold,10'":
                "set label 'DOS (states/eV/unit-cell)' rotate center at screen 0.04,0.5":
--                spinHead:
                []
        tPP = PlotPlate thead "unset label" tot
    let pdos  = chunksOf 2
              $ map (susunOrbs "dos" foldernya tailer invStat) daftarCetak
        phead = "" -- spinHead
        pdosPP = addHeaderToLast ("set label 'Energy - E_F (eV)' at graph 0.5,-0.25 center")
               $ addHeaderToLast ("set format x '% h';set xtics font 'Times New Roman,10' nomirror offset -.15,.6 out;")
               $ zipWith (\(_,(_,a,_)) p ->
                    addHeader ("set label '" ++ (replace "#" " " a) ++ "' at graph 0.80,0.92 font 'Times New Roman Bold,10'") p ) daftarCetak'

               $ map (PlotPlate phead "unset label") pdos
        allPP = (tPP:pdosPP)
    putStrLn $ "====" ++ show daftarCetak'

    return $ unlines $ map plot allPP
plotStatementDOS _ = return "====Error: plotStatementDOS @CPVO/IO/Plot/Gnuplot/DOS.hs:30"

showXtics p = addHeader ("set format x '% h';set xtics font 'Times New Roman,10' nomirror offset -.15,.6 out;") p
addHeaderToLast s ps = concat [init ps, [ addHeader s $ last ps]]

insertSpinLabel :: (Num a, Ord a) => a -> String -> String
insertSpinLabel ymin =
  if ymin < 0 then
              (.) (insertLabel "spin-up" "at graph 0.15,0.9 font ',10'")
                  (insertLabel "spin-down" "at graph 0.15,0.1 font ',10'")
              else id

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
susunOrbs job foldernya tailer invStat ((urutan,((jumlah,nomor,_),_,listOrbital)),spin) = unwords [
                                        Text.Printf.printf "'%s/%s.isp%d.site%03d.%s' u ($1*rydberg):((%s)*%d*(%d)*(%d)/rydberg ) w l ls %d notitle"
                                          (T.pack foldernya)
                                          job
                                          spin
                                          nomor
                                          (T.pack tailer)
                                          ("$" ++ (intercalate "+$" $ delta (listOrbital /= []) listOrbital $ map show ([2..26] :: [Integer])))
                                          jumlah
                                          invStat
                                          (delta (spin < 2) 1 (-1) :: Int)
                                          urutan
                                          ]


susunTot :: String -> String -> Int -> Integer -> String
susunTot foldernya tailer invStat spin = unwords [
                                        Text.Printf.printf "'%s/dos.tot.%s' u ($1*rydberg):($%d *( %d ) *( %d ) / rydberg ) w l lc rgb 'black' notitle"
                                          (T.pack foldernya)
                                          (T.pack tailer)
                                          (delta (spin < 2) 2 (3) :: Int)
                                          invStat
                                          (delta (spin < 2) 1 (-1) :: Int)
                                          ]

addHeader :: String -> PlotPlate -> PlotPlate
addHeader s p@(PlotPlate h _ _) = p { _head = unlines [s,h]}

plot :: PlotPlate -> String
plot (PlotPlate h t p) =
  let ps = "plot " ++ (intercalate "," p)
  in  unlines $ [h, ps, t]

data PlotPlate = PlotPlate { _head :: String
                           , _tail :: String
                           , _plot :: [String]
                           } deriving Show

prependToAllWith :: (t -> a) -> [t] -> [a]
prependToAllWith _ [] = []
prependToAllWith f (x:xs) = (f x) : prependToAllWith f xs

intercalateWith :: (a -> a) -> [a] -> [a]
intercalateWith _ [] = []
intercalateWith f (x:xs) = x : prependToAllWith f xs
