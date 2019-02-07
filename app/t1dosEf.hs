#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Maybe
import Data.Char (ord)
import Data.List.Split
import Control.Monad.IO.Class (MonadIO)


import CPVO.Numeric
import CPVO.IO
--import CPVO.IO.Reader.Ecalj.Common
--import CPVO.IO.Reader.Ecalj.DOS


import qualified System.Process as SP
import Text.Printf as TP
import System.IO (openTempFile,hClose)




import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List
-------------------------

import Numeric.LinearAlgebra hiding (find)
-------------------------

rydberg :: Double
rydberg=1/13.605

testArgs :: [String]
testArgs = concat [ [ "GWtable01.tex"
                   , "Total and partial DOS at *E*~F~ in states/eV/unit-cell, QSGW method."
                   , "|*N*$_\\uparrow$(*E*~F~)|*N*$_\\downarrow$(*E*~F~)|*N*(*E*~F~)"
                     ]
                   , words "@{}lSSS@{} -9:6 25 T o flipSpin nico2o4 extendedNiCo2O4.normal/nico2o4.6G10 Ni:Ni#3d:6:7:9:8:10 Co:Co#3d:6:7:8:9:10 O:O#2p:3:4:5"
                   ]

main :: IO ()
main = do
    allArgs <- getArgs
    --Right (invStat, ymax, xmin, xmax, ctrlAtoms, uniqAtoms, ctrlAtomicAOs,jdTable, jdHeads, foldernya, tailer, colAlign, texFile) <- readHeaderData allArgs
    Right all@(invStat,_,_,_,_,_, ctrlAtomicAOs,jdTable, jdHeads, foldernya, tailer,_, texFile,targetPrint) <- readHeaderData allArgs
    putStrLn $ "===========allArgs =========" ++ show all
    totalDOS <- readTotalDOSText tailer foldernya
    let resTotY0 = map (\i -> getY0 $ totalDOS ¿ [0,i]) $ flipBy invStat [1,2]
    putStrLn "===========NF total DOS========="
    putStrLn $ show resTotY0
    let newInvStat = getInvStat invStat
    pdosAtAll <- readPDOS newInvStat tailer foldernya
                  [ Cetak s a | s <- [1,2], a <- ctrlAtomicAOs ]
    --putStrLn $ "===pdosAtAll " ++ show pdosAtAll
    putStrLn "=========!readPDOS"
    let rs' = rendertable
              $ (:) jdHeads
              $ (:)  ("Total" : (map (showDouble 3) $ processCols $ map ((* rydberg) . last . toList) $ resTotY0))
              $ map go3
              $ groupBy (\a b -> fst a == fst b)
              $ sortBy (\a b -> compare (fst a) (fst b) )
              $ map go $ concat
              $ map go2
              $ groupBy (\(_,s) (_,s') ->  (spinID s) == (spinID s'))
              $ map (\(mp,b) -> ((* rydberg) $ last $ toList $ getY0 mp, b)) pdosAtAll
    putStrLn $ "====rs " ++ rs'
    let rs = unlines [ rs' , jdTable ]
    T.writeFile texFile $ T.pack rs
      where
        go3 [(l,(_,u)),(_,(_,d))] = (l:(map (showDouble 3) $ processCols [u,d]))
        go2 = groupBy (\(_,s) (_,s') ->  (labelAO $ atom s) == (labelAO $ atom s'))
        go :: [(Double,Cetak)] -> (String,(SpinID,Double))
        go a@((_, (Cetak s (AO _ _ labelAt _))):_) = (labelAt,(s, sum $ map fst a))
        processCols [u,d] = [u,d,u+d]
        processCols _ = []


{-
    res' <- markdownToTex rs
    let res = T.replace "\\}" "}"
            $ T.replace "\\{" "{"
            $ T.pack $ unlines  [ "\\begin{longtable}[]{" ++ colAlign ++ "}"
                                , unlines $ tail $ lines $ T.unpack res'
                                ]
    T.writeFile texFile res
    putStrLn $ texFile ++ "=============done:t1dosEf.hs=========="
      where
        sumIt x [] = x
        sumIt [u0,d0] ([up,dn]:xs) = sumIt [u0+up,d0+dn] xs
        sumIt _ _ = []
    -}

--import CPVO.Numeric
--import CPVO.IO
----
--import Numeric.LinearAlgebra
-- ===============================================

getPDOS' :: Matrix Double -> String -> [Int] -> [String] -> IO (Matrix Double)
getPDOS' res _ _ []  = return res
getPDOS' res tmpf intAOs (nf:nfiles)  = do
  _ <- SP.system $ "mkdir -p temp; more +2 " ++ nf ++ " > " ++ tmpf
  aPDOS' <- fmap (\x -> sumRow $ (¿) x intAOs) $ loadMatrix tmpf
  getPDOS' (fromBlocks [[res,asColumn aPDOS']]) tmpf intAOs nfiles

-------------------------------------------------------------
-- Input Processing: read PDOS data
--

type Directory = String
data InvStat = Keep | Flip deriving (Show)

getInvStat a = if a < 0 then Flip
                        else Keep


readPDOS :: InvStat -> String -> Directory -> [Cetak]
         -> IO [(Matrix Double, Cetak)]
readPDOS invStat tailer dir ctrlAtAOs = do
  putStrLn "========readPDOS"
  putStrLn $ "====ctrlAtAOs " ++ show ctrlAtAOs
  putStrLn $ "===invStat " ++ show invStat
  putStrLn $ "===tailer" ++ show tailer
  sequence [ readOnePDOS dir tailer j | j <- ctrlAtAOs ]
--    (head $ readPDOS' invStat dir tailer) (head ctrlAtAOs))
--  (\x ->  [f a | f <- (readPDOS' invStat dir tailer), a <- x]) $ head ctrlAtAOs
  --sequence
  --  $ (\x ->  [f a | f <- (readPDOS' invStat dir tailer), a <- x]) ctrlAtAOs

  {-
readPDOS' :: InvStat -> Directory -> String ->
             [ Cetak -> IO (Matrix Double, Cetak)]
readPDOS' invStat foldernya tailer = fmap (readOnePDOS foldernya tailer) $ flipByI invStat [1,2] -- spin 1 up n spin 2 down
-}

flipByI Keep x = x
flipByI Flip x = reverse x

--readPDOS :: String -> String -> Int -> ((String, String, [Int]), [(Int, String)]) -> IO (Int,String, Matrix Double)
--readPDOS ::(spin, (noAt, (symAt, (labelAt, PDOS :: Matrix Double))))
readOnePDOS :: String
            -> String -> Cetak
            -> IO (Matrix Double, Cetak )
readOnePDOS theFolder tailing (Cetak spin atTarget@(AO noAt symAt labelAt intAOs)) = do
  putStrLn "=========readOnePDOS@src/CPVO/IO/Reader/Ecalj/DOS.hs"
  let namaFao = theFolder ++ "/dos.isp" ++ show spin ++ ".site" ++ (TP.printf "%03d" noAt) ++ "." ++ tailing
  (tmpfile,h) <- openTempFile "temp" "aEDOS.suffix"
  hClose h
  _ <- SP.system $ "mkdir -p temp; sed '{1d}' " ++ namaFao ++ " > " ++ tmpfile
  aoE <- fmap (\x -> (¿) x [0]) $ loadMatrix $ tmpfile -- 0th column, Energy column
  let zeroE = asColumn $ konst 0 (rows aoE)                                      -- zero valued column instead of real column
  aPDOS <- fmap (dropColumns 1) $ getPDOS' zeroE tmpfile intAOs [namaFao]                                -- create sum of per atomic AOs (PDOS/atom)
  putStrLn "=========readOnePDOS@src/CPVO/IO/Reader/Ecalj/DOS.hs"
  return $ (fromBlocks [[aoE, aPDOS]] , (Cetak spin (atTarget { labelAO = hashSpaceText labelAt} )))
  --return $ (fromBlocks [[aoE, aPDOS]] , (spin, (hashSpaceText labelAt, (noAt, symAt))))
------------------------------------------------------------------

-- getPDOS :: String -> String -> Int -> ((String, String, [Int]), [(Int, String)]) -> IO (Int,String, Matrix Double)
-- getPDOS theFolder tailing spin (a@(namaAtom,jdAtom,intAOs),lsAtoms) = do
--   let namaFaos = map (\(x,_) -> theFolder ++ "/dos.isp" ++ show spin ++ ".site" ++ (TP.printf "%03d" x) ++ "." ++ tailing) lsAtoms
--   (tmpfile,h) <- openTempFile "temp" "aEDOS.suffix"
--   hClose h
--   _ <- inshell2text $ "mkdir -p temp; more +2 " ++ (head namaFaos) ++ " > " ++ tmpfile -- this is needed only to generate aoE, the real processing is in getPDOS'
--   aoE <- fmap (\x -> (¿) x [0]) $ loadMatrix tmpfile                             -- 0th column, Energy column
--   let zeroE = asColumn $ konst 0 (rows aoE)                                      -- zero valued column instead of real column
--   aPDOS <- fmap (dropColumns 1) $ getPDOS' zeroE tmpfile intAOs namaFaos                                -- create sum of per atomic AOs (PDOS/atom)
--   return $ (spin, hashSpaceText jdAtom, fromBlocks [[aoE, aPDOS]])
-- ------------------------------------------------------------------
--
--import CPVO.Numeric
--import CPVO.IO
--
--import qualified Data.Text as T
--import qualified Data.Text.IO as T
--import Data.List
--import Numeric.LinearAlgebra

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

readUniqAtoms :: [String] -> [UniqueAtom]
readUniqAtoms allAtoms =
          map (\a -> UA (length a) (snd $ head a) (fst $ head a)) $
          groupBy (\a b -> fst a == fst b) $
          zip  allAtoms ([1..] :: [Int])

type SpinID = Integer

-- daftarCetak :: [((notCtk,((count,noFrstAtom,atomSymbol),label  ,[    AOs ])),spin)]
-- daftarCetak :: [(( 1    ,((  2  ,    13    ,  "Ni"    ),"Ni#2p",["3","4","5"])),1)]
genDaftarCetak :: [UniqueAtom] -> String -> String -> [String]
--               -> IO [((Integer, (UniqueAtom, String, [String])),SpinID)]
                ->  IO [Cetak]
genDaftarCetak availableAtoms _ _ aos = do
  return [ Cetak i j | j <- map d3 d2 , i <- [1,2] ]
--  return $ [ (i,j) | i <- daftarCetak' listAtoms , j <- [1,2] ]
  where
    daftarCetak' lAtoms = zip [1..]
                      $ map (\(a,label,b) -> (head $ filter (\(_,_,aa) -> aa == (T.pack a)) lAtoms , label, b) ) d1
    d1 = map ( (\(a:label:as) -> (a,label,as)) . splitOn ":") aos
    d2 = map ( (\(a:label:as) -> AO 0 a label (map (read :: String -> Int) as)) . splitOn ":") aos
    d3 x = let Just tA = find (\y -> atsymUA y == atsym x) availableAtoms
            in x {atnum = atnumUA tA}

data Cetak = Cetak { spinID :: SpinID
                   , atom :: AtOrb
                   } deriving Show

-- uniqAtoms : [(count,noFirstAtom,atomicSymbol)]
data UniqueAtom = UA { count :: Int
                     , atnumUA :: AtNum
                     , atsymUA :: AtSym
                     } deriving (Show)

type AtNum = Int
type AtSym = String
data AtOrb = AO { atnum :: AtNum
                , atsym :: AtSym
                , labelAO :: String
                , intAOs :: [Int]
                } deriving (Show,Eq)

-- ctrlAtomicAOs :: [(atomNumber,(atomSym,(label ,[intAOs])))]
-- ctrlAtomicAOs :: [(    1     ,(  "O"  ,("O#2p",[ 2,3,4])))]
-- ctrlAtomicAOs :: 14 atoms
genCtrlAtomicAOs :: [(String, String, [Int])]
                 -> [T.Text]
                 -- -> [(Int, (String, (String, [Int])))]
                 -> [AtOrb]
genCtrlAtomicAOs aoSet ctrlAtoms =  map genAtOrb $ map (\x -> (head $ takeAOs x aoSet))
          $ concat
          $ groupBy (\(_,a:_) (_,b:_) -> (ord a) == (ord b))
          $ zip ([1..]::[Int]) $ map T.unpack ctrlAtoms


genAtOrb :: (Int, (String, (String, [Int]))) -> AtOrb
genAtOrb (a,(b,(c,d))) = AO a b c d

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
                      , [UniqueAtom]
                      , [AtOrb]
                      , String, [String], String, String, String, String, [Cetak]))
readHeaderData all@(texFile:jd:jdHead:colAlign:xr:ymax':_:_:invS:tailer:foldernya:aos) = do
  -------------------------------reading data------------------------
    putStrLn "=========readHeaderData@CPVO.IO.Reader.Ecalj.Common"
    putStrLn $ (++) "===allArgs==" $ unlines $ map show all
    let invStat = if (invS == "flipSpin") then (-1) else 1
    let ymax = read ymax' :: Double
    let [xmin,xmax] = map (read :: String -> Double) $ splitOn ":" xr
    ctrlAtoms <- readCtrlAtoms tailer foldernya
    let jdHeads = splitOn "|" jdHead
    let uniqAtoms = readUniqAtoms $ map T.unpack ctrlAtoms
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
    let ctrlAtomicAOs = genCtrlAtomicAOs aoSet ctrlAtoms
    let jdTable = "Table: " ++ jd
    putStrLn $ "===ctrlAtomicAOs " ++ show ctrlAtomicAOs
    putStrLn "========!readHeaderData@CPVO.IO.Reader.Ecalj.Common"
    let ret = (invStat, ymax, xmin, xmax, ctrlAtoms, uniqAtoms, ctrlAtomicAOs,jdTable, jdHeads, foldernya, tailer, colAlign, texFile,targetPrint)
    putStrLn $ "====ret" ++ show ret
    return $ Right ret

readHeaderData _ = return $ Left
  "===Error:readHeaderData@CPVO/IO/Reader/Common wrong args ========"
