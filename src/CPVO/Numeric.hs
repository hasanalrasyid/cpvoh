{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module CPVO.Numeric (
  integrateAll,
  sumRow,
  getY0,
  delta,
  integrateToZero,
  integrateAtomicPDOS
  ) where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data hiding (find)
import Data.List (findIndex,groupBy)
import Data.Maybe (fromJust)

integrateToZero mP = integrateAll 0
  $ (++ ([toList $ getY0 $ takeColumns 2 mP]))
  $ takeWhile (\(a:_) -> a <= 0)
  $ toLists $ takeColumns 2 mP

integrateAll res [] = res
integrateAll res ([enA,nA]:b@[enB,nB]:as)
  | as == [] = integrateAll (res + (enB - enA)*(nA+nB)*0.5) []
  | otherwise = integrateAll (res + (enB - enA)*(nA+nB)*0.5) (b:as)

sumRow a = a #> konst 1 (cols a)

getY0 dos = getY0' lowPos higNeg
  where
    rTDOS = toRows $ dos
    highestNeg = (+) (-1) $ fromJust $ findIndex (\a -> (atIndex a 0) >= 0) rTDOS
    lowestPos = highestNeg + 1
    higNeg = rTDOS !! highestNeg
    lowPos = rTDOS !! lowestPos

getY0' a b = a + (scale m v)
  where
    v = b - a
    m = ((*) (-1) $ a ! 0) / ((b ! 0) - (a ! 0))

delta :: Bool -> b -> b -> b
delta x y z = if x then y else z

integrateAtomicPDOS pdosAtomicPilihan =
          (\[us,ds] -> zipWith (\(iu,b) (idown,_) -> (iu,idown,b)) us ds )
          $ groupBy (\(_,(s,_)) (_,(s',_)) -> s == s') -- [[(spin,label,iup)]]
          $ map (\(mp,b) -> (integrateToZero mp,b)) $ pdosAtomicPilihan

