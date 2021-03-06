{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

import CPVO.IO.Reader.Ecalj.MMOM (getMMOM)

import System.Environment (getArgs)

main :: IO ()
main = do
  allArgs <- getArgs
  putStrLn $ show allArgs
  {-
  let testArgs = [ "table03.tex"
                 ,"Atomic magnetic moment"
                 ,"{Atom Number}|{Atomic Orbital}|{Spin Density Integration}|{PDOS Integration}|{Difference}"
                  --colAlign like @{}llSSS@{}
                  , "@{}llSSS@{}"
                 ,"-9:6","25","T","o","1","nico2o4"
                 ,"extendedNiCo2O4.normal/nico2o4.0GGA"
                 ,"Ni:Ni#3d:6:7:9:8:10","Co:Co#3d:6:7:8:9:10","O:O#2p:3:4:5" ]
  -}
  getMMOM allArgs
  putStrLn "========beresbanh======="
