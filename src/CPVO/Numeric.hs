module CPVO.Numeric (
  testFunc,
  integrateAll
  ) where

testFunc = do
  putStrLn "========test========="

integrateAll res [] = res
integrateAll res ([enA,nA]:b@[enB,nB]:as)
  | as == [] = integrateAll (res + (enB - enA)*(nA+nB)*0.5) []
  | otherwise = integrateAll (res + (enB - enA)*(nA+nB)*0.5) (b:as)
