{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Test (
    hiHask
  , sumRootsInH
  ) where

import Foreign -- Ptr
import Foreign.C -- CDouble

foreign export ccall sumrootsinh_ :: Ptr Int -> Ptr CDouble -> Ptr CDouble -> IO ()
foreign export ccall sumRootsInH :: Ptr Int -> Ptr CDouble -> Ptr CDouble -> IO ()
foreign export ccall hihask_ :: IO ()
foreign export ccall hiHask :: IO ()

hihask_ = hiHask
sumrootsinh_ = sumRootsInH

hiHask :: IO ()
hiHask = putStrLn "=====HASK======"

sumRootsInH :: Ptr Int -> Ptr CDouble -> Ptr CDouble -> IO ()
sumRootsInH n' xs' result = do
  n <- peek n'
  putStrLn $ "============" ++ (show n)
  xs <- peekArray n xs'
  poke result $ sumRoots xs
  return ()

sumRoots :: [CDouble] -> CDouble
sumRoots xs = sum (map sqrt xs)

testArgs :: [String]
testArgs = concat [ [ "GWtable01.tex"
                   , "Total and partial DOS at *E*~F~ in states/eV/unit-cell, QSGW method."
                   , "|*N*$_\\uparrow$(*E*~F~)|*N*$_\\downarrow$(*E*~F~)|*N*(*E*~F~)"
                     ]
                   , words "@{}lSSS@{} -9:6 25 T o flipSpin nico2o4 extendedNiCo2O4.normal/nico2o4.6G10 Ni:Ni#3d:6:7:9:8:10 Co:Co#3d:6:7:8:9:10 O:O#2p:3:4:5"
                   ]


