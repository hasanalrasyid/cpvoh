
{--
#!/usr/bin/env stack
-- stack script --resolver hasanCust.yaml -v
  --package hascpvo --package turtle --package foldl --package text --package process --package boxes --package hmatrix --package text-format
-- stack --resolver hasanCust.yaml -v script --verbose
-- stack --resolver hasanCust.yaml script --verbose --package pandoc --package haddock-library --package attoparsec
-- stack --install-ghc runghc --resolver bin/hasanCust.yaml --package text --package text-format --package pandoc --package haddock-library
--}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

import Turtle hiding (sortBy,char,text)
import Lib (someFunc)
import CPVO.Numeric (integrateAll,testFunc,getY0,delta)
import CPVO.IO (showDouble,markdownToTex,shell2list,table,getPDOS,pdosA')

--import Turtle.Helper
import qualified Control.Foldl as Fold
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
--import qualified Data.Text.Format as T
import Text.Printf as TP
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Either
-------------------------

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Devel (readMatrix)
import Numeric.LinearAlgebra.Data hiding (find)
import Data.Char
import System.Process as SP
import System.IO
-------------------------
import Text.PrettyPrint.Boxes hiding ((<>),cols,rows)
import qualified Text.PrettyPrint.Boxes as TB
import Data.List
-- ===============================================
-- start of Accelerate
-- import Data.Array.Accelerate              as A
-- import Data.Array.Accelerate.LLVM.Native  as CPU
-- import Data.Array.Accelerate.LLVM.PTX     as GPU
--
{-
dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)

main1 = do
    kj@(texFile:jd:xr:ymax':wTot:tumpuk:invS:tailer:foldernya:aos) <- getArgs
    putStrLn $ show kj
--    let xs = fromList (Z:.10) [0..]   :: Vector Float
--    let ys = fromList (Z:.10) [1,3..] :: Vector Float
--    CPU.run $ dotp (use xs) (use ys)
-}

{-
shell2list :: MonadIO io =>  Shell a->  io [a]
shell2list xx = fold (xx) Fold.list

pad width x = x ++ replicate k ' '
  where k = width - length x

fmt_column :: [String] -> Box
fmt_column items = vcat left (addHead $ map (text.pad width) items)
  where width = maximum $ map length items
        hsep = text ( replicate width '-' )
        addHead (a:as) = a:hsep:as

table :: [[String]] -> Box
table rs = vsep TB.<> hcat top (intersperse vsep (map fmt_column columns)) TB.<> vsep
  where

    columns = transpose rs
    nrows = length rs
    vsep =  vcat left $ map char ("|" ++ (concat $ replicate nrows "|"))

delta :: Bool -> b -> b -> b
delta x y z = if x then y else z

caller :: T.Text
caller = T.unlines  [ "callme with : genPDOSAtomicOrbital.hs [Atom:Orbital]                tailer    folder"
                    , "ex.         : genPDOSAtomicOrbital.hs 'O NiTd:2:3:4:5 CoTd:2:3:4:5' nico2o4   nico2o4.invB.0GGA"
                    ]

getIntegralDOS = id
-}

main = do
    (texFile:jd:jdHead:xr:ymax':wTot:tumpuk:invS:tailer:foldernya:aos) <- getArgs
    fCtrl <- T.readFile $ foldernya ++ "/ctrl." ++ tailer
    -------------------------------start calculation------------------------
      -------------------------------generating data------------------------
    let invStat = read invS :: Int
    let ymax = read ymax' :: Double
        [xmin,xmax] = map (read :: String -> Double) $ splitOn ":" xr
      -------------------------------generating DOS data------------------------
    totalDOS <- loadMatrix $ foldernya ++ "/dos.tot." ++ tailer
      -------------------------------integrating DOS data------------------------
    let intgTot = map (\ii -> integrateAll 0
                              $ (++ ([toList $ getY0 ii])) $ takeWhile (\(a:_) -> a <= 0)
                              $ toLists ii) $ map (\i -> totalDOS ¿ [0,i]) [1,2] -- run it on spin [1,2]
    putStrLn $ show intgTot

      -------------------------------integrating PDOS data------------------------
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
          zip  ctrlAtoms [1..]
    -- daftarCetak : [(nourut,,jumlah,nourut,symbol)]
    let daftarCetak'  = zip [1..]
                      $ map (\(a,label,b) -> (head $ filter (\(_,_,aa) -> aa == (T.pack a)) uniqAtoms , label, b) )
                      $ map ( (\(a:label:as) -> (a,label,as)) . splitOn ":") aos
        daftarCetak = [ (i,j) | i <- daftarCetak' , j <- [1,2] ]
      -------------------------------generating PDOS data------------------------
              -- map ditambah -1 karena input mengikuti gnuplot
              -- input : d kolom 6-10
              -- gnuplot : d kolom 6-10
              -- hmatrix : d kolom 5-9
    let aoSet = map ( (\(a:l:as) -> (a,l,map ( ((+) (-1)) . read :: String -> Int) as) ) . splitOn ":") aos
    pdosA <- sequence $ (\x ->  [f a | f <- (pdosA' foldernya tailer), a <- x])
              -- ((namaAtom,jdAtom,[intAOs]),[(nourutAtom,namaAtom)])
              $ map (\x@((_,i):_) -> (head $ takeAO i aoSet ,x) )
              $ groupBy (\(_,a:_) (_,b:_) -> (ord a) == (ord b))
              $ zip ([1..]::[Int]) $ map T.unpack ctrlAtoms
      -------------------------------integrating PDOS data------------------------
    let intgPdosA =  map (\(s,j,mP) -> (s,j,integrateAll 0
                                            $ (++ ([toList $ getY0 $ takeColumns 2 mP]))
                                            $ takeWhile (\(a:_) -> a <= 0)
                                            $ toLists $ takeColumns 2 mP  )) pdosA   -- we only consider the first 2 columns, i.e. Energy, PDOS of 1st Atom
      -------------------------------generating MarkDown Representation------------------------
    let rIntgAll' =
          render $ table
          $ (:) (splitOn "|" jdHead)
          $ (:) ((:) "Total" $ map (showDouble 2) $ (\[a,b] -> [a,b,a-b]) $ intgTot)
          $ map (\[(_,_,u),(_,j,d)] ->  j : map (showDouble 2) [u,d,u-d])
          $ groupBy (\(_,a,_) (_,b,_) -> a == b)
          $ sortBy (\(_,a,_) (_,b,_) ->  compare a b ) intgPdosA
    let rIntgAll = unlines  [
                            rIntgAll'
                            , "Table: " ++ jd
                            ]
    putStrLn rIntgAll
      -------------------------------generating LaTex Representation------------------------
    resIntAll' <- markdownToTex rIntgAll
--    T.putStrLn $ resIntAll'
    let resIntAll = T.replace "\\}" "}"
                  $ T.replace "\\{" "{" $ T.pack
                  $ unlines [
                            "\\begin{longtable}[]{@{}lSSS@{}}"
                            , unlines $ tail $ lines $ T.unpack resIntAll'
                            ]
--    putStrLn $ show intgPdosA
--    T.writeFile texFile resIntAll
    mmomSD <- readMMOM nAtom foldernya
    putStrLn $ show mmomSD
    putStrLn $ show $ length pdosA
    {-
    pdosAtomic <- sequence $ (\x ->  [f a | f <- (pdosA' foldernya tailer), a <- x])
              -- ((namaAtom,jdAtom,[intAOs]),[(nourutAtom,namaAtom)])
              -- ((String , String,[ Int  ]),[(Int       , String )])
              -- (("O"    ,"O#2p" ,[2,3,4 ]),[(1         ,"O"     )])
      -}
    pdosAtomic <- sequence $ (\x ->  [f a | f <- (pdosA' foldernya tailer), a <- x]) $ map (\x@(_,i) -> ((head $ takeAO i aoSet),[x]) )
              $ concat
              $ groupBy (\(_,a:_) (_,b:_) -> (ord a) == (ord b))
              $ zip ([1..]::[Int]) $ map T.unpack ctrlAtoms
--    putStrLn $ show pdosAtomic
    putStrLn $ show $ length pdosAtomic
    testFunc
    someFunc
    putStrLn "====done===="
      where
        getAllPDOS (s,n,pd) = do
          let pdTot = getY0 pd
          return (s,n,pdTot)


takeAO i [] = []
takeAO i (a@(n,_,_):as) = if (i == n) then [a]
                                     else takeAO i as

readMMOM nAtom foldernya = do
    fLLMF <- T.readFile $ foldernya ++ "/llmf"
    (tmpfile,h) <- openTempFile "temp" "llmf_mmom.suffix"
    hClose h
    let fMMOM = inshell ( T.pack $ concat [ "mkdir -p temp; grep mmom ", foldernya , "/llmf "
                                            ,"| tail -n", show (nAtom + 1)
                                            ,"| head -n", show nAtom
                                            ,"| awk '{print $2}'"
--                                            ,"> ", tmpfile -- this is needed only to generate aoE, the real processing is in getPDOS'
                                          ]) empty
    mmom <- shell2list fMMOM
    return
      $ map fst $ rights
      $ map T.double
      $ map lineToText $ mmom



{-
pdosA' :: String -> String -> [((String, String, [Int]), [(Int, String)]) -> IO (Int,String, Matrix Double)]
pdosA' foldernya tailer = fmap (getPDOS foldernya tailer) [1,2]
---------------------------------------------------
hashSpaceText t = T.unpack $ T.replace "#" " " $ T.pack t
---------------------------------------------------
sumRow a = a #> konst 1 (cols a)
getPDOS' res _ _ []  = return res
getPDOS' res tmpf intAOs (nf:nfiles)  = do
  _ <- SP.system $ "mkdir -p temp; more +2 " ++ nf ++ " > " ++ tmpf
--  aoDOS' <- fmap (\x -> (¿) x intAOs) $ loadMatrix tmpf
  aPDOS' <- fmap (\x -> sumRow $ (¿) x intAOs) $ loadMatrix tmpf
  getPDOS' (fromBlocks [[res,asColumn aPDOS']]) tmpf intAOs nfiles
-------------------------------------------------------------

getPDOS :: String -> String -> Int -> ((String, String, [Int]), [(Int, String)]) -> IO (Int,String, Matrix Double)
getPDOS theFolder tailing spin (a@(namaAtom,jdAtom,intAOs),lsAtoms) = do
  let namaFaos = map (\(x,_) -> theFolder ++ "/dos.isp" ++ show spin ++ ".site" ++ (TP.printf "%03d" x) ++ "." ++ tailing) lsAtoms
  (tmpfile,h) <- openTempFile "temp" "aEDOS.suffix"
  hClose h
  _ <- SP.system $ "mkdir -p temp; more +2 " ++ (head namaFaos) ++ " > " ++ tmpfile -- this is needed only to generate aoE, the real processing is in getPDOS'
  aoE <- fmap (\x -> (¿) x [0]) $ loadMatrix tmpfile                             -- 0th column, Energy column
  let zeroE = asColumn $ konst 0 (rows aoE)                                      -- zero valued column instead of real column
  aPDOS <- fmap (dropColumns 1) $ getPDOS' zeroE tmpfile intAOs namaFaos                                -- create sum of per atomic AOs (PDOS/atom)
--  putStrLn $ show $ sumRow aPDOS
--  let pDOS = sumRow aPDOS -- create sum of aPDOS (atomic PDOS/cell)
--  return $ (spin, hashSpaceText jdAtom, fromBlocks [[aoE, asColumn pDOS]])
  return $ (spin, hashSpaceText jdAtom, fromBlocks [[aoE, aPDOS]])
------------------------------------------------------------------

getPDOSori :: String -> String -> Int -> ((String, String, [Int]), [(Int, String)]) -> IO (Int,String, Matrix Double)
getPDOSori theFolder tailing spin (a@(namaAtom,jdAtom,intAOs),lsAtoms) = do
  let namaFaos = map (\(x,_) -> theFolder ++ "/dos.isp" ++ show spin ++ ".site" ++ (TP.printf "%03d" x) ++ "." ++ tailing) lsAtoms
  (tmpfile,h) <- openTempFile "temp" "aEDOS.suffix"
  hClose h
  _ <- SP.system $ "mkdir -p temp; more +2 " ++ (head namaFaos) ++ " > " ++ tmpfile
  aoE <- fmap (\x -> (¿) x [0]) $ loadMatrix tmpfile
  let zeroE = asColumn $ konst 0 (rows aoE)
  pDOS <- fmap sumRow $ getPDOS' zeroE tmpfile intAOs namaFaos
  return $ (spin, hashSpaceText jdAtom, fromBlocks [[aoE, asColumn pDOS]])
------------------------------------------------------------------

-- getY0 dos = getY0' lowPos higNeg
--   where
--     rTDOS = toRows $ dos
--     highestNeg = (+) (-1) $ fromJust $ findIndex (\a -> (atIndex a 0) >= 0) rTDOS
--     lowestPos = highestNeg + 1
--     higNeg = rTDOS !! highestNeg
--     lowPos = rTDOS !! lowestPos
--
-- getY0' a b = a + (scale m v)
--   where
--     v = b - a
--     m = ((*) (-1) $ a ! 0) / ((b ! 0) - (a ! 0))
--

-}
