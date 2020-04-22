#!/usr/bin/env stack
--stack --resolver lts-14.27 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

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

shell2list = id

caller :: T.Text
caller = T.unlines  [ "callme with   genPBAND.hs folder spin invStat atom@orbital-atom@orbital"
                    , "ex.         : genPBAND.hs string 1/2  0/1(inv) '4@t2g-10@p'"
                    , "ex.         : genPBAND.hs nico2o4.invB.0GGA 1 0 '4@t2g-10@p'"
                    ]
main = do
    (bw:s:i:a:f) <- getArgs
    if s == "-h" then T.putStrLn caller else main1 bw s i a f

main1 oldBw spin invStat atomNos foldernya = do
    let daftaratomOs =  map (splitOn "@") $ splitOn "-" atomNos
        daftarJudulSpinFolders = filter (/=[""]) $ map (splitOn "@") $ splitOn ":" $ unwords foldernya
        folder = last $ last daftarJudulSpinFolders
        daftarAOJSF = zip [1..] $ concat $ map (\a -> zip daftaratomOs $ repeat a) daftarJudulSpinFolders
--    putStrLn $ show $ daftarJudulSpinFolders
--    putStrLn $ show daftaratomOs
--    putStrLn $ show $ head daftarAOJSF
{- sementara
-}
    let err' = map ( \(_,([o,a],[j,s,folder])) -> inshell2text $ concat [ "cd ", folder, "; BandWeight.py PROCAR.",cekSpin s invStat "1" "UP" "DN" , " ",a, " ", o]) daftarAOJSF
    _ <- if (oldBw /= "1") then mapM shell2list err' else return []
--    let lTitik' = inshell $ T.pack $ concat ["for i in nico2o4.invB.11G20/bnd*spin1 ; do sed -e '/^#\\| $/d' $i| sed -e 1b -e '$!d' | awk '{print $2}';done|sort -u "] empty
--    lTitik <- mapM shell2list lTitik'
--    putStrLn $ show lTitik

--    let err'' = map (\[o,a] -> inshell (T.pack $ concat [ "cd ", folder, "; BandWeight.py PROCAR.",cekSpin spin invStat "1" "UP" "DN" , " ",a, " ", o]) empty ) daftaratomOs
--    _ <- mapM shell2list err''
--    putStrLn $ show err
{--
  (\$2>6.08991818?0.4+\$2:\$2):(\$2>6.08991818&&\$2<6.09991818?1/0:\$3)
--}
--      $ show
    putStrLn $ concat $ intercalate [", "]
        $ map ( \(i,([o,a],[j,s,folder])) -> ["'",folder,"/bw.",a,".",o,".PROCAR.",cekSpin s invStat "1" "UP" "DN" ,".dat' u ($1>6.09971818?0.4+$1:$1):2:($1>6.08991818&&$1<6.09991818?0:$3*3) ls ",show (i)," ps variable title '",concat $ intersperse "." [o,a,j],"'"] :: [String] )
        $ daftarAOJSF
--    map () daftaratomOs
--      $ splitOn "-" "9@t2g-10@p"


cekSpin s "0" ok sOk sNo = if s == ok then sOk else sNo
cekSpin s _ ok sOk sNo = if s == ok then sNo else sOk





