{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module CPVO.IO.Reader.Ecalj.Common where
import CPVO.Numeric
import CPVO.IO

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Either (rights)
import Numeric.LinearAlgebra
import Data.Char (ord)

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
readUniqAtoms allAtoms =
          map (\a -> (length a, snd $ head a, fst $ head a)) $
          groupBy (\a b -> fst a == fst b) $
          zip  allAtoms [1..]

-- daftarCetak :: [((notCtk,((count,noFrstAtom,atomSymbol),label  ,[    AOs ])),spin)]
-- daftarCetak :: [(( 1    ,((  2  ,    13    ,  "Ni"    ),"Ni#2p",["3","4","5"])),1)]
genDaftarCetak listAtoms tailer foldernya aos = do
  return $ [ (i,j) | i <- daftarCetak' listAtoms , j <- [1,2] ]
  where
    daftarCetak' listAtoms = zip [1..]
                      $ map (\(a,label,b) -> (head $ filter (\(_,_,aa) -> aa == (T.pack a)) listAtoms , label, b) )
                      $ map ( (\(a:label:as) -> (a,label,as)) . splitOn ":") aos

-- ctrlAtomicAOs :: [(atomNumber,(atomSym,(label ,[intAOs])))]
-- ctrlAtomicAOs :: [(    1     ,(  "O"  ,("O#2p",[ 2,3,4])))]
-- ctrlAtomicAOs :: 14 atoms
genCtrlAtomicAOs aoSet ctrlAtoms =  map (\x -> (head $ takeAOs x aoSet))
          $ concat
          $ groupBy (\(_,a:_) (_,b:_) -> (ord a) == (ord b))
          $ zip ([1..]::[Int]) $ map T.unpack ctrlAtoms

-- totalDOS :: Matrix Double [ energy, DOSspinUp, DOSspinDown ]
readTotalDOSText tailer foldernya = loadMatrix $ foldernya ++ "/dos.tot." ++ tailer

-----------------------------------------------------------
getLastLLMF foldernya = inshell2text $ concat ["ls -laht ", foldernya,"/llmf{,_gwscend.*} | head -1|awk '{print $NF}'" ]
-----------------------------------------------------------

readHeaderData (texFile:jd:jdHead:colAlign:xr:ymax':wTot:tumpuk:invS:tailer:foldernya:aos) = do
  -------------------------------reading data------------------------
    let invStat = if (invS == "flipSpin") then (-1) else 1
    let ymax = read ymax' :: Double
    let [xmin,xmax] = map (read :: String -> Double) $ splitOn ":" xr
    ctrlAtoms <- readCtrlAtoms tailer foldernya
    let nAtom = length ctrlAtoms
    let jdHeads = splitOn "|" jdHead
    let uniqAtoms = readUniqAtoms ctrlAtoms
    putStrLn $ show ctrlAtoms
    putStrLn $ show uniqAtoms
    -- daftarCetak : [(nourut,,jumlah,nourut,symbol)]
    daftarCetak <- genDaftarCetak uniqAtoms tailer foldernya aos
    putStrLn $ show aos
    putStrLn $ show $ last daftarCetak
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
    putStrLn $ show $ head ctrlAtomicAOs
--    pdosAtomicPilihan <- readPDOS invStat tailer foldernya $ take 2 ctrlAtomicAOs
--    let integratedAtomicPDOS = integrateAtomicPDOS pdosAtomicPilihan
--    putStrLn $ show $ integratedAtomicPDOS
    putStrLn "===done:readHeaderData@CPVO/IO/Reader/Common ====================="
    return
      (invStat, ymax, xmin, xmax, ctrlAtoms, uniqAtoms, ctrlAtomicAOs,jdTable, jdHeads, foldernya, tailer, colAlign, texFile)

