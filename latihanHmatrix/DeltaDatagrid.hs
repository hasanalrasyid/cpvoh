{-# FlexibleContexts #-}
--import Lib
--import CPVO
--import CPVO.Parser
--import CPVO.Data
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import System.Environment
import Text.Printf (printf)
import Data.List

import Control.Monad (replicateM, when)
import Data.Traversable (for)
import System.Environment (getArgs)

main :: IO ()
main = do
  -- ers we are looking for
  [t1f,t2f] <- getArgs
  t1 <- readFile t1f
  t2 <- readFile t2f
  let tt1 = map read $ concat $ map words $ lines t1 :: [Double]
      tt2 = map read $ concat $ map words $ lines t2 :: [Double]
  let beda = zipWith (-) tt2 tt1
  let kurangnya = 8 - (mod (length beda) 8)
  putStrLn $ format "  " (printf "%.6E") $ matrix 8 $ ( zipWith (-) tt2 tt1 ) ++ take kurangnya [0,0..]


