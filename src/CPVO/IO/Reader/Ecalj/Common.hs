{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module CPVO.IO.Reader.Ecalj.Common where
import CPVO.Numeric (integrateAll,getY0,delta,integrateToZero)
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

-- ctrlAtomicAOset :: [((atomSym,label ,[intAOs]),(atomNumber,atomSym))]
-- ctrlAtomicAOset :: [((  "O"  ,"O#2p",[2,3,4 ]),(     1    ,  "O"  ))]
-- ctrlAtomicAOset :: 14 atoms
genCtrlAtomicAOset aoSet ctrlAtoms =  map (\x@(_,i) -> ((head $ takeAO i aoSet),[x]))
          $ concat
          $ groupBy (\(_,a:_) (_,b:_) -> (ord a) == (ord b))
          $ zip ([1..]::[Int]) $ map T.unpack ctrlAtoms

-- ctrlAtomicAOset :: [(atomNumber,(atomSym,(label ,[intAOs])))]
-- ctrlAtomicAOset :: [(    1     ,(  "O"  ,("O#2p",[ 2,3,4])))]
-- ctrlAtomicAOset :: 14 atoms
genCtrlAtomicAOs aoSet ctrlAtoms =  map (\x -> (head $ takeAOs x aoSet))
          $ concat
          $ groupBy (\(_,a:_) (_,b:_) -> (ord a) == (ord b))
          $ zip ([1..]::[Int]) $ map T.unpack ctrlAtoms

-- totalDOS :: Matrix Double [ energy, DOSspinUp, DOSspinDown ]
readTotalDOSText tailer foldernya = loadMatrix $ foldernya ++ "/dos.tot." ++ tailer


-----------------------------------------------------------
getLastLLMF foldernya = inshell2text $ concat ["ls -lahut ", foldernya,"/llmf{,_gwscend.*} | head -1|awk '{print $NF}'" ]
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
    (tMMomSD:mmomSD) <- readMMOM nAtom foldernya
    putStrLn $ show $ map (showDouble 3) mmomSD
      -------------------------------generating PDOS data------------------------
              -- map ditambah -1 karena input mengikuti gnuplot
              -- input : d kolom 6-10
              -- gnuplot : d kolom 6-10
              -- hmatrix : d kolom 5-9

              -- ((namaAtom,jdAtom,[intAOs]),[(nourutAtom,namaAtom)])
              -- ((String , String,[ Int  ]),[(Int       , String )])
              -- (("O"    ,"O#2p" ,[2,3,4 ]),[(1         ,"O"     )])
      -}
    -- let ctrlAtomicAOset = genCtrlAtomicAOset aoSet ctrlAtoms
    let ctrlAtomicAOs = genCtrlAtomicAOs aoSet ctrlAtoms
    let jdTable = "Table: " ++ jd
    putStrLn $ show $ head ctrlAtomicAOs
    pdosAtomicPilihan <- readPDOS tailer foldernya $ take 2 ctrlAtomicAOs
    let integratedAtomicPDOS = integrateAtomicPDOS pdosAtomicPilihan
    putStrLn $ show $ integratedAtomicPDOS
    putStrLn "===done:readHeaderData@CPVO/IO/Reader/Common ====================="
    return
      (invStat, ymax, xmin, xmax, ctrlAtoms, uniqAtoms, ctrlAtomicAOs,jdTable, jdHeads, foldernya, tailer)

integrateAtomicPDOS pdosAtomicPilihan =
          (\[us,ds] -> zipWith (\(iu,b) (idown,_) -> (iu,idown,b)) us ds )
          $ groupBy (\(_,(s,_)) (_,(s',_)) -> s == s') -- [[(spin,label,iup)]]
          $ map (\(mp,b) -> (integrateToZero mp,b)) $ pdosAtomicPilihan
    {-
    putStrLn "==========================="
    pdosAtomic <- sequence
      $ (\x ->  [f a | f <- (pdosA' foldernya tailer), a <- x]) ctrlAtomicAOset
      -------------------------------integrating PDOS data------------------------
    let integratedAtomicPDOS =
          (\[us,ds] -> zipWith (\(_,j,iu) (_,_,idown) -> (j, iu,idown)) us ds )
          $ groupBy (\(s,_,_) (s',_,_) -> s == s') -- [[(spin,label,iup)]]
          $ map (\(a,x,mp) -> (a,x,integrateToZero mp)) $ pdosAtomic
    putStrLn $ show $ length ctrlAtomicAOset
    putStrLn $ show integratedAtomicPDOS
    putStrLn "========"
    putStrLn $ show tMMomSD
    let rIntgAll' = rendertable
          $ (:) (splitOn "|" jdHead)
          $ (:) (concat [ ["Total" ]
                        , ["  "]
                        , map (showDouble 3) $ (\[t,iu,id] -> [t,iu-id,t-(iu-id)]) $ (tMMomSD:intgTot)
                        --, map (showDouble 2) $ (\[iu,id] -> [iu,id,(iu-id)]) $ (intgTot)
                        ])
          $ zipWith (\a b -> a:b) (map show [1,2..])
          $ zipWith (\sdMom (j,iu,id) -> j:(map (showDouble 3) [sdMom,iu-id,sdMom-(iu-id)])) mmomSD integratedAtomicPDOS
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
    -- putStrLn rIntgAll
    -- T.putStrLn resIntAll
--    T.writeFile texFile resIntAll
--    -}

{-
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
-}
