{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-} -- untuk let x :: Int = 5

module CPVO.IO.Plot.PDOS
  where

import CPVO.IO
import CPVO.IO.Plot.Gnuplot.DOS
import CPVO.IO.Plot.Gnuplot.Common

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List.Split
import System.Process as SP

--plotPDOS plotStatementDOS_args@(jd:xr:ymax':wTot:tumpuk:invS:tailer:foldernya:aos) = do
  --invStat : spin inversal... it means switching between up n down spin
  --          flipSpin : flip it
  --          otherwise: keep it straight
  --
plotPDOS :: [String] -> IO ()
plotPDOS (fOut:xr:yr:over:invStat:poskey':total:foldernya:daftarOrbital) = do
  putStrLn "====start: CPVO.IO.Plot.PDOS===="
-------------draft---------------------------------------
  let poskey = unwords $ splitOn ":" poskey'
  tailer <- fmap (T.unpack . last . T.splitOn "." . head ) $ inshell2text $ unwords [ "ls", foldernya ++ "/dos.tot.*" ]
  putStrLn $ show poskey
  -- tailer="$(ls "${dirs[0]}" |grep dos.tot | awk -F '.' '{print $NF}' )"
  -- over=$3
  -- mkdir -p plots
  let ender = genEnder
  let akhiran = unlines [ ender
                        , "system 'rm -f plots/tmp* && for i in {eps,pdf,png}; do mv plots/hasil.$i " ++ fOut ++ ".$i; done '"
                        ]
  let plotplate = "set format x '% h'; set xtics format '' nomirror ; unset xlabel; unset ylabel "
  let topTitle' = (last $ T.splitOn "." $ T.pack foldernya)
  let topTitle = case topTitle' of
                      "0GGA" -> "GGA(PBE)"
                      _ -> concat [ "QSGW_{"
                                  , T.unpack $ last $ T.splitOn "G" topTitle'
                                  , "}"
                                  ]
      --generator="f1.genPDOSAtomicOrbitalTot.hs $topTitle $xr $yr $total $over $invStat $tailer $foldernya $daftarOrbital"

  plotplate1 <- plotStatementDOS (topTitle:xr:yr:total:over:invStat:tailer:foldernya:daftarOrbital)
  putStrLn "==========================================="
  T.writeFile "temp.glt" $ T.pack $ unlines [ genTOP [xr,yr,poskey]
                     , plotplate
                     , plotplate1
                     , akhiran
                     ]
  _ <- SP.system "gnuplot temp.glt"
    -- #---#dimensi=$(convert plots/hasil.jpg -fuzz 5% -transparent white sparse-color:-|sed -e 's/ /\n/g'|awk -F ',' 'BEGIN{a=0; b=0;aa=10000;bb=10000}{if (a<$1) a=$1; if ($1<aa) aa=$1;  if (b<$2) b=$2; if (bb>$2) bb=$2 }END{print a-(10-a%10)"x"b-bb+(10-b%10)"+"aa-(30+aa%10)"+"bb-(10-aa%10)}')
    -- #---#convert plots/hasil.jpg -crop $dimensi plots/hasil.jpg
    -- #---##convert plots/hasil.jpg -pointsize 24 -font "monofur" label:'Energy (eV)' -gravity Center -append plots/hasil.jpg
    -- #---##convert plots/hasil.jpg -gravity West -font monofur -pointsize 24 -draw 'rotate -90 text 0,20 "DOS (states/eV)"' plots/hasil.jpg

  ------------------------------
  putStrLn "====finish: CPVO.IO.Plot.PDOS===="
plotPDOS _ = do
  putStrLn "====Error: CPVO.IO.Plot.PDOS : incomplete Arguments===="
  putStrLn $ unwords $ "./f1 invA.gga -9:9 -1:25 1 keepSpin top:right T":
                       "ext.nico2o4.normal/tio2.g0.post":
                       "Ti:Ti#3d:6:7:9:8:10":
                       "O:O#2p:3:4:5":[]