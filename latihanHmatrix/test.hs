import Numeric.LinearAlgebra

main = do
  let a = (1000><1000) $ replicate (1000*1000) (1::Double)
      b = konst 1 (1000,1000)
  print $ a@@>(100,100)
  print $ b@@>(100,100)
  print $ (a <> b) @@> (900,900)
