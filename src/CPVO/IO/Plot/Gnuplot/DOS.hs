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
plotStatementDOS (jd:xr:ymax':wTot:_:invS:tailer:foldernya:aos) = do
    fCtrl <- T.readFile $ foldernya ++ "/ctrl." ++ tailer
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

    let hasilTot' = if (wTot == "T") then intercalate "," $ map (susunTot foldernya tailer invStat ) ([1,2] :: [Integer]) else ""
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



