import Control.Parallel
import Data.Word

collatzLen :: Int -> Word32 -> Int
collatzLen c 1 = c
collatzLen c n | n `mod` 2 == 0 = collatzLen (c+1) $ n `div` 2
               | otherwise      = collatzLen (c+1) $ 3*n+1

pmax x n = x `max` (collatzLen 1 n, n)

main :: IO ()
main = putStrLn "cekthis"



{-
main = print soln
   where
      solve xs = foldl pmax (1,1) xs
      s1 = solve [2..500000]
      s2 = solve [500001..999999]
      soln = s2 `par` (s1 `pseq` max s1 s2)
      -}
