{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module CPVO.IO.Reader.Ecalj.Util
  where

import Data.Maybe
import Data.Char (ord)
import Data.List.Split
import Control.Monad.IO.Class (MonadIO)
--import Data.Either (rights)

--import CPVO.Numeric
import CPVO.IO
import CPVO.IO.Type

--import qualified System.Process as SP
--import Text.Printf as TP
--import System.IO (openTempFile,hClose)




--import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List
-------------------------

import Numeric.LinearAlgebra hiding (find)
-------------------------

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

readUniqAtoms :: [T.Text] -> [UniqueAtom]
readUniqAtoms allAtoms =
          map (\a -> UA (length a) (snd $ head a) (fst $ head a)) $
          groupBy (\a b -> fst a == fst b) $
          zip  allAtoms ([1..] :: [Int])

-- daftarCetak :: [((notCtk,((count,noFrstAtom,atomSymbol),label  ,[    AOs ])),spin)]
-- daftarCetak :: [(( 1    ,((  2  ,    13    ,  "Ni"    ),"Ni#2p",["3","4","5"])),1)]
genDaftarCetak :: [UniqueAtom] -> String -> String -> [String]
--               -> IO [((Integer, (UniqueAtom, String, [String])),SpinID)]
                ->  IO [Cetak]
genDaftarCetak availableAtoms _ _ aos = do
  return [ Cetak i j | j <- map d3 d2 , i <- [1,2] ]
  where
    d2 = map ( (\(a:label:as) ->
                    AO 0 (T.pack a) label (map (read :: String -> Int) as))
               . splitOn ":") aos
    d3 x = let Just tA = find (\y -> atsymUA y == atsym x) availableAtoms
            in x {atnum = atnumUA tA}

-- ctrlAtomicAOs :: [(atomNumber,(atomSym,(label ,[intAOs])))]
-- ctrlAtomicAOs :: [(    1     ,(  "O"  ,("O#2p",[ 2,3,4])))]
-- ctrlAtomicAOs :: 14 atoms
genCtrlAtomicAOs :: [(String, String, [Int])]
                 -> [T.Text]
                 -> [AtOrb]
genCtrlAtomicAOs aoSet ctrlAtoms =
  [ AO n s l is | let lt = zip (map T.unpack ctrlAtoms) $ ([1..] :: [Int])
                , (s',l,is) <- aoSet
                , let s = T.pack s'
                , (_,n) <- filter (\(b,_) -> b == s') lt
  ]


genAtOrb :: (Int, (String, (String, [Int]))) -> AtOrb
genAtOrb (a,(b,(c,d))) = AO a (T.pack b) c d

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
               -> IO ( Either String ( InvStat, Double, Double, Double, [T.Text]
                      , [UniqueAtom]
                      , [AtOrb]
                      , String, [String], String, String, String, String, [Cetak]))
readHeaderData al@(texFile:jd:jdHead:colAlign:xr:ymax':_:_:invS:tailer:foldernya:aos) = do
  -------------------------------reading data------------------------
    putStrLn "=========readHeaderData@CPVO.IO.Reader.Ecalj.Common"
    putStrLn $ (++) "===allArgs==" $ unlines $ map show al
    let invStat = if (invS == "flipSpin") then Flip else Keep
    let ymax = read ymax' :: Double
    let [xmin,xmax] = map (read :: String -> Double) $ splitOn ":" xr
    ctrlAtoms <- readCtrlAtoms tailer foldernya
    let jdHeads = splitOn "|" jdHead
    let uniqAtoms = readUniqAtoms $ ctrlAtoms
    putStrLn $ "===ctrlAtoms " ++ show ctrlAtoms
    putStrLn $ "===uniqAtoms " ++ show uniqAtoms
    -- daftarCetak : [(nourut,,jumlah,nourut,symbol)]
    targetPrint <- genDaftarCetak uniqAtoms tailer foldernya aos
    putStrLn $ "===aos " ++ show aos
    putStrLn $ "===targetPrint " ++ show targetPrint
      -------------------------------generating DOS data------------------------
--    totalDOS <- readTotalDOSText tailer foldernya
      -------------------------------integrating DOS data------------------------
--    let intgTot = map (\i -> integrateToZero $ totalDOS ¿ [0,i]) [1,2] -- run it on spin [1,2]
--    putStrLn $ show intgTot
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
    debugIt "aoSet ===  " aoSet
    let ctrlAtomicAOs = genCtrlAtomicAOs aoSet ctrlAtoms
    let jdTable = "Table: " ++ jd
    putStrLn $ "===ctrlAtomicAOs " ++ show ctrlAtomicAOs
    putStrLn "========!readHeaderData@CPVO.IO.Reader.Ecalj.Common"
    let ret = (invStat, ymax, xmin, xmax, ctrlAtoms, uniqAtoms, ctrlAtomicAOs,jdTable, jdHeads, foldernya, tailer, colAlign, texFile,targetPrint)
    putStrLn $ "====ret" ++ show ret
    return $ Right ret

readHeaderData _ = return $ Left
  "===Error:readHeaderData@CPVO/IO/Reader/Common wrong args ========"
