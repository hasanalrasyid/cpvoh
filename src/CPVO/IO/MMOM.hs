{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module CPVO.IO.MMOM where
--x-- import Turtle hiding (sortBy,char,text)
import CPVO.Numeric (integrateAll,getY0,delta,integrateToZero)
import CPVO.IO
--x--
--x-- --import Turtle.Helper
--x-- import qualified Control.Foldl as Fold
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
--x-- --import qualified Data.Text.Format as T
--x-- import Text.Printf as TP
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Either (rights)
--x-- -------------------------
import Numeric.LinearAlgebra
--x-- import Numeric.LinearAlgebra.Devel (readMatrix)
--x-- import Numeric.LinearAlgebra.Data hiding (find)
import Data.Char (ord)
--x-- import System.Process as SP
--x-- import System.IO
--x-- -------------------------
--x-- import Text.PrettyPrint.Boxes hiding ((<>),cols,rows)
--x-- import qualified Text.PrettyPrint.Boxes as TB
--x-- import Data.List
--x-- -- ===============================================
-- start of Accelerate
-- import Data.Array.Accelerate              as A
-- import Data.Array.Accelerate.LLVM.Native  as CPU
-- import Data.Array.Accelerate.LLVM.PTX     as GPU
--

-- main = do
--   allArgs <- getArgs
--   putStrLn $ show allArgs
--   let testArgs = [ "table03.tex"
--                  ,"Atomic magnetic moment"
--                  ,"|{Spin Density Integration}|{PDOS Integration}|{Difference}"
--                  ,"-9:6","25","T","o","1","nico2o4"
--                  ,"extendedNiCo2O4.normal/nico2o4.0GGA"
--                  ,"Ni:Ni#3d:6:7:9:8:10","Co:Co#3d:6:7:8:9:10","O:O#2p:3:4:5" ]
--
--   --main' allArgs
--   getMMOM testArgs

getMMOM (texFile:jd:jdHead:colAlign:xr:ymax':wTot:tumpuk:invS:tailer:foldernya:aos) = do
    fCtrl <- T.readFile $ foldernya ++ "/ctrl." ++ tailer
    -------------------------------start calculation------------------------
      -------------------------------generating data------------------------
    let invStat = read invS :: Int
    let ymax = read ymax' :: Double
        [xmin,xmax] = map (read :: String -> Double) $ splitOn ":" xr
      -------------------------------generating DOS data------------------------
    totalDOS <- loadMatrix $ foldernya ++ "/dos.tot." ++ tailer
      -------------------------------integrating DOS data------------------------
    let intgTot = map (\i -> integrateToZero $ totalDOS ¿ [0,i]) [1,2] -- run it on spin [1,2]
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
    let intgPdosA =  map (\(s,j,mP) -> (s,j,integrateToZero $ takeColumns 2 mP  )) pdosA   -- we only consider the first 2 columns, i.e. Energy, PDOS of 1st Atom
    putStrLn "========"
    (tMMomSD:mmomSD) <- readMMOM nAtom foldernya
    putStrLn $ show $ map (showDouble 3) mmomSD
    putStrLn $ show $ length pdosA
    {-
    pdosAtomic <- sequence $ (\x ->  [f a | f <- (pdosA' foldernya tailer), a <- x])
              -- ((namaAtom,jdAtom,[intAOs]),[(nourutAtom,namaAtom)])
              -- ((String , String,[ Int  ]),[(Int       , String )])
              -- (("O"    ,"O#2p" ,[2,3,4 ]),[(1         ,"O"     )])
      -}
    pdosAtomic <- sequence
      $ (\x ->  [f a | f <- (pdosA' foldernya tailer), a <- x])
      $ map (\x@(_,i) -> ((head $ takeAO i aoSet),[x]))
      $ concat
      $ groupBy (\(_,a:_) (_,b:_) -> (ord a) == (ord b))
      $ zip ([1..]::[Int]) $ map T.unpack ctrlAtoms
    let integratedAtomicPDOS =
          (\[us,ds] -> zipWith (\(_,j,iu) (_,_,id) -> (j, iu - id)) us ds )
          $ groupBy (\(s,_,_) (s',_,_) -> s == s')
          $ map (\(a,x,mp) -> (a,x,integrateToZero mp)) $ pdosAtomic
    putStrLn $ show tMMomSD
    let rIntgAll' = rendertable
          $ (:) (splitOn "|" jdHead)
          $ (:) (concat [ ["Total" ]
                        , ["  "]
                        , map (showDouble 3) $ (\[t,iu,id] -> [t,iu-id,t-(iu-id)]) $ (tMMomSD:intgTot)
                        --, map (showDouble 2) $ (\[iu,id] -> [iu,id,(iu-id)]) $ (intgTot)
                        ])
          $ zipWith (\a b -> a:b) (map show [1,2..])
          $ zipWith (\sdMom (j,intMom) -> j:(map (showDouble 3) [sdMom,intMom,sdMom-intMom])) mmomSD integratedAtomicPDOS
    let rIntgAll = unlines  [
                            rIntgAll'
                            , "Table: " ++ jd
                            ]
    resIntAll' <- markdownToTex rIntgAll
    let resIntAll = T.replace "\\}" "}"
                  $ T.replace "\\{" "{" $ T.pack
                  $ unlines [
                            "\\begin{longtable}[]{" ++ colAlign ++ "}"
                            , unlines $ tail $ lines $ T.unpack resIntAll'
                            ]
    putStrLn rIntgAll
    T.putStrLn resIntAll
    T.writeFile texFile resIntAll
    putStrLn "===done==="

readMMOM nAtom foldernya = do
    fLLMF <- T.readFile $ foldernya ++ "/llmf"
    mmom <- fmap (map T.double) $ inshell2text $ concat [ "mkdir -p temp; grep mmom ", foldernya , "/llmf "
                                            ,"| tail -n", show (nAtom + 1)
                                            ,"| head -n", show nAtom
                                            ,"| awk '{print $2}'"
                                          ]
    sdtMMOM <- fmap (map T.double) $ inshell2text $ concat [ "grep mmom ", foldernya , "/llmf | grep ehf | tail -1 | sed -e 's/^.*mmom=//g'| awk '{print $1}'"
                                          ]


    return ( map fst $ rights $ concat [sdtMMOM,mmom])

