{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module CPVO.IO.Reader.Ecalj.Common where
import CPVO.Numeric
import CPVO.IO

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List.Split
import Data.List
import Data.Maybe
import Numeric.LinearAlgebra
import Data.Char (ord)
import Control.Monad.IO.Class (MonadIO)

readCtrlAtoms :: String -> String -> IO [T.Text]
readCtrlAtoms tailer foldernya = do
    fCtrl <- T.readFile $ foldernya ++ "/ctrl." ++ tailer
    return $ catMaybes
      $ map ( T.stripPrefix "ATOM=" .  head)
      $ filter (/=[])
      $ map ( T.words . T.takeWhile (/='#') )
      $ head
      $ splitWhen (T.isPrefixOf "SPEC")
      $ last $ splitWhen (T.isPrefixOf "SITE")
      $ T.lines fCtrl

    -- uniqAtoms : [(count,noFirstAtom,atomicSymbol)]
type CntN1Atsym = (Int,Int,T.Text)
readUniqAtoms :: [T.Text] -> [CntN1Atsym]
readUniqAtoms allAtoms =
          map (\a -> (length a, snd $ head a, fst $ head a)) $
          groupBy (\a b -> fst a == fst b) $
          zip  allAtoms ([1..] :: [Int])

-- daftarCetak :: [((notCtk,((count,noFrstAtom,atomSymbol),label  ,[    AOs ])),spin)]
-- daftarCetak :: [(( 1    ,((  2  ,    13    ,  "Ni"    ),"Ni#2p",["3","4","5"])),1)]
genDaftarCetak :: [CntN1Atsym] -> String -> String -> [String]
               -> IO [((Integer,(CntN1Atsym,String,[String])),Integer)]
genDaftarCetak listAtoms _ _ aos = do
  return $ [ (i,j) | i <- daftarCetak' listAtoms , j <- [1,2] ]
  where
    daftarCetak' lAtoms = zip [1..]
                      $ map (\(a,label,b) -> (head $ filter (\(_,_,aa) -> aa == (T.pack a)) lAtoms , label, b) )
                      $ map ( (\(a:label:as) -> (a,label,as)) . splitOn ":") aos

-- ctrlAtomicAOs :: [(atomNumber,(atomSym,(label ,[intAOs])))]
-- ctrlAtomicAOs :: [(    1     ,(  "O"  ,("O#2p",[ 2,3,4])))]
-- ctrlAtomicAOs :: 14 atoms
genCtrlAtomicAOs :: [(String, String, [Int])]
                 -> [T.Text]
                 -> [(Int, (String, (String, [Int])))]
genCtrlAtomicAOs aoSet ctrlAtoms =  map (\x -> (head $ takeAOs x aoSet))
          $ concat
          $ groupBy (\(_,a:_) (_,b:_) -> (ord a) == (ord b))
          $ zip ([1..]::[Int]) $ map T.unpack ctrlAtoms

-- totalDOS :: Matrix Double [ energy, DOSspinUp, DOSspinDown ]
readTotalDOSText :: String -> String -> IO (Matrix Double)
readTotalDOSText tailer foldernya = loadMatrix $ foldernya ++ "/dos.tot." ++ tailer

-----------------------------------------------------------
getLastLLMF :: MonadIO io =>
  String -> io [T.Text]
getLastLLMF foldernya = inshell2text $ concat ["ls -laht ", foldernya,"/llmf{,_gwscend.*} | head -1|awk '{print $NF}'" ]
-----------------------------------------------------------

--readHeaderData (texFile:jd:jdHead:colAlign:xr:ymax':wTot::invS:tailer:foldernya:aos) = do
readHeaderData :: [String]
               -> IO ( Either String ( Double, Double, Double, Double, [T.Text]
                      , [CntN1Atsym]
                      , [(Int, (String, (String,[Int])))]
                      , String, [String], String, String, String, String))
readHeaderData (texFile:jd:jdHead:colAlign:xr:ymax':_:_:invS:tailer:foldernya:aos) = do
  -------------------------------reading data------------------------
    putStrLn "=========readHeaderData@CPVO.IO.Reader.Ecalj.Common"
    let invStat = if (invS == "flipSpin") then (-1) else 1
    let ymax = read ymax' :: Double
    let [xmin,xmax] = map (read :: String -> Double) $ splitOn ":" xr
    ctrlAtoms <- readCtrlAtoms tailer foldernya
    let jdHeads = splitOn "|" jdHead
    let uniqAtoms = readUniqAtoms ctrlAtoms
    putStrLn $ "===ctrlAtoms " ++ show ctrlAtoms
    putStrLn $ "===uniqAtoms " ++ show uniqAtoms
    -- daftarCetak : [(nourut,,jumlah,nourut,symbol)]
    daftarCetak <- genDaftarCetak uniqAtoms tailer foldernya aos
    putStrLn $ "===aos " ++ show aos
    putStrLn $ "===daftarCetak " ++ show daftarCetak
      -------------------------------generating DOS data------------------------
    totalDOS <- readTotalDOSText tailer foldernya
      -------------------------------integrating DOS data------------------------
    let intgTot = map (\i -> integrateToZero $ totalDOS ¿ [0,i]) [1,2] -- run it on spin [1,2]
    putStrLn $ show intgTot
    let aoSet = map ( (\(n:l:as) -> (n,l,map ( ((+) (-1)) . read :: String -> Int) as) ) . splitOn ":") aos
    {-
      -------------------------------generating PDOS data------------------------
              -- map ditambah -1 karena input mengikuti gnuplot
              -- input : d kolom 6-10
              -- gnuplot : d kolom 6-10
              -- hmatrix : d kolom 5-9

              -- ((namaAtom,jdAtom,[intAOs]),[(nourutAtom,namaAtom)])
              -- ((String , String,[ Int  ]),[(Int       , String )])
              -- (("O"    ,"O#2p" ,[2,3,4 ]),[(1         ,"O"     )])
      -}
    let ctrlAtomicAOs = genCtrlAtomicAOs aoSet ctrlAtoms
    let jdTable = "Table: " ++ jd
    putStrLn $ "===ctrlAtomicAOs " ++ show ctrlAtomicAOs
--    pdosAtomicPilihan <- readPDOS invStat tailer foldernya $ take 2 ctrlAtomicAOs
--    let integratedAtomicPDOS = integrateAtomicPDOS pdosAtomicPilihan
--    putStrLn $ show $ integratedAtomicPDOS
    putStrLn "========!readHeaderData@CPVO.IO.Reader.Ecalj.Common"
    return $ Right
      (invStat, ymax, xmin, xmax, ctrlAtoms, uniqAtoms, ctrlAtomicAOs,jdTable, jdHeads, foldernya, tailer, colAlign, texFile)

readHeaderData _ = return $ Left
  "===Error:readHeaderData@CPVO/IO/Reader/Common wrong args ========"
