{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

import CPVO.IO.Plot.PDOS

import System.Environment (getArgs)

main :: IO ()
main = do
  allArgs <- getArgs
  {-
  let testArgs1 = [ "fig2"
                 , "-9:2"
                 , "55" , "o" ,"1", "top:right", "T"
                 , "extendedNiCo2O4.normal/nico2o4.1G0"
                 , "Ni:Ni#3d:6:7:9:8:10", "Co:Co#3d:6:7:8:9:10", "O:O#2p:3:4:5"
                 ]
  let testArgsOld = [ "QSGW_{0}"
                 , "-9:2", "55", "T"
                 , "o", "1"
                 , "nico2o4"
                 , "extendedNiCo2O4.normal/nico2o4.0GGA"
                 ,"Ni:Ni#3d:6:7:9:8:10", "Co:Co#3d:6:7:8:9:10", "O:O#2p:3:4:5"
                 ]
  let testArgs = [ "QSGW_{0}"
                 , "-9:2", "55", "T"
                 , "o", "1"
                 , "nico2o4"
                 ,"Ni:Ni#3d:6:7:9:8:10/Co:Co#3d:6:7:8:9:10/O:O#2p:3:4:5"
                 , "extendedNiCo2O4.normal/nico2o4.0GGA"
                 , "extendedNiCo2O4.normal/nico2o4.0GW"
                 , "extendedNiCo2O4.normal/nico2o4.11GW20"
                 ]
  -}
--  plotPDOS allArgs
  putStrLn "========beres======="
