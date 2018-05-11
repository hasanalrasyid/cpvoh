{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module CPVO.IO.Plot.PDOS
  where

import CPVO.Numeric
import CPVO.IO
import CPVO.IO.DOS
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
plotPDOS (fOut:xr:yr:over:invStat:poskey':total:foldernya:daftarOrbital) = do
  putStrLn "====start: CPVO.IO.Plot.PDOS===="
-------------draft---------------------------------------
  let poskey = unwords $ splitOn ":" poskey'
  let xticsnya = "-100,2,100"
  let yticsnya = concat ["-", yr, "+5, 5,", yr, "-5"]
  -- yticsnya = (echo $yr | awk '{print "-"$1"+5,5,"$1"-5"}')
  tailer <- fmap (T.unpack . last . T.splitOn "." . head ) $ inshell2text $ unwords [ "ls", foldernya ++ "/dos.tot.*" ]
  let mainTitle = ""
  putStrLn $ show poskey
  -- tailer="$(ls "${dirs[0]}" |grep dos.tot | awk -F '.' '{print $NF}' )"
  -- over=$3
  -- mkdir -p plots
  let tempGLT = genTOP [xr,yr,poskey]
  let ender = genEnder
  let pre = "plot "
  let akhiran = unlines [ ender
                        , "system 'cd plots && rm -f tmp*jpg && for i in {eps,pdf,png}; do mv hasil.$i " ++ fOut ++ ".$i; done '"
                        ]
  let plotplate = "set format x '% h'; set xtics format '' nomirror ; unset xlabel; unset ylabel "
  let urutan = 0
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

