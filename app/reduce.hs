#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import System.Environment
import Text.Printf (printf)

--import Control.Monad.IO.Class (MonadIO, liftIO)
--
import Control.Applicative
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Machine hiding (zipWith)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import System.IO (isEOF)
import Control.Monad (unless,replicateM_)

--import Data.Machine.Mealy
--import Data.Machine.Stack
import Data.List

main :: IO ()
main = do
--  res <- runT pipeline
  -- Remember that runT collects outputs in a list
--  putStrLn $ format "  " (printf "%.6f") $ fromRows res
  --res <- runT $ teeT zipping (source [1..]) lineSource
  (origin':_) <- getArgs
  let origin = read origin' :: Int
  res <- runT $ lineSource
              ~> skipHeader 2
              ~> echo
  let (lat',(atList:atNums':_:res')) = splitAt 3 res
  let atNums = map read $ words $ B8.unpack atNums' :: [Int]
  let (lat:coords:_) = map (fromColumns . map getVec) [lat',res']
  let convLat' = ["3.7850000858  0.0000000000  0.0000000000"
                 ,"0.0000000000  3.7850000858  0.0000000000"
                 ,"0.0000000000  0.0000000000  9.5195999146"
                 ]
  let convLat = fromColumns $ map getVec convLat'
--  putStr $ dispf 6 lat
--  putStr $ dispf 6 convLat
  -- consistently column based
  -- id: primLat_origin = convLat_origin . primLat_convLat
  --     P_o            = C_o            . P_C
  --
  -- putStr $ (++) "P_o =" $ dispf 6 $ convLat <> latFrac
  let latFrac = (inv convLat) <> lat
  let vOrigin = if (origin <= 0)  then 3 |> [0,0,0]
                                 else head $ toColumns $ coords ¿ [origin - 1]
  let newCoords = fromRows $ map (\a -> a - vOrigin) $ toColumns coords
  let filteredCoords =  (flip (<>)) lat $
                        fromRows $
                        filter cekFirstQuadrant $
                        --filter cek1Quadrant $ filter (\x -> norm_2 x <= sqrt 3) $
        toRows $ newCoords <> (inv lat)
  let atoms = concat $ zipWith (replicate) atNums $ words $ B8.unpack atList
  let atomsNcoords = zip atoms $ toColumns coords
  let newAtomsDef =  concatMap (\x -> filter (sameVec ("",x)) atomsNcoords) $
        toRows filteredCoords
  let newAtoms = (\x -> zip (map head x) $ map length x) $
        group $ map fst newAtomsDef
          {-
  putStrLn "Reduced Cell"
  putStrLn "1.0"
  putStr $ unlines $ tail $ lines $ dispf 6 $ tr' lat
  putStrLn $ unwords $ map fst newAtoms
  putStrLn $ unwords $ map (show . snd) newAtoms
  putStrLn "Cartesian"
  putStr $ unlines $ tail $ lines $ dispf 6 $ filteredCoords
--  putStr $ unlines $ tail $ lines $ dispf 6 $ filteredCoords <> (inv lat)
--  putStr $ unlines $ tail $ lines $ dispf 6 $ tr' $ (inv lat) <> filteredCoords
--  putStr $ show filteredCoords
  putStr $ unlines $ map show newAtomsDef
  let calonCoords = (¿) coords $ findIndices firstQuadrant $ toRows $ newCoords <> (inv lat)
  let calonCoords2 = (flip (<>)) lat $ fromRows $ filter firstQuadrant $ toRows $ newCoords <> (inv lat)
  putStr $ show calonCoords
  putStrLn $ show calonCoords2
  putStrLn "======================================="
  -}
  let movedCoords = fromColumns $ map (\a -> a - vOrigin) $ toColumns coords
  let primLat_O = lat
  let primCoords = fromColumns $ -- nubBy cekEdge2 $
                   removeMirrorEdge $
                   filter (not . isCorner) $
                   filter cekFirstQuadrant $
                   toColumns $ (inv primLat_O) <> movedCoords
  let primAtoms =  concatMap (\x -> filter (sameVec ("",x)) atomsNcoords) $
        toColumns $ lat <> primCoords
  putStrLn "Reduced Cell"
  putStrLn "1.0"
  putStr $ onlyMatrix $ tr' lat
  putStrLn $ unwords $ map head $ group $ map fst primAtoms
  putStrLn $ unwords $ map (show . length) $ group $ map fst primAtoms
  putStrLn "Cartesian"
  putStr $ onlyMatrix $ tr' $ lat <> primCoords
    {-
  putStrLn "C"
  putStr $ unlines $ map show primAtoms
  putStrLn $ unlines $ map show $
                   removeMirrorEdge $
                   filter (not . isCorner) $
                   filter cekFirstQuadrant $
                   toColumns $ (inv primLat_O) <> movedCoords
-}

removeMirrorEdge i =
  let a = [(x,y) | x <- i, y <- i]
      d (v,w,_) = sort [v,w]
      c = map head $ nub $ map d $ filter (\(_,_,c) -> c) $ map cek3 a
   in i \\ c
cek3 (a,b) = (a,b,t1 a b)

cek1 x = [ (c,a - b) | a <- x , b <- x, let c = elem True $ map (t1 (a - b)) p001 ]

v011 :: Vector Double
v011 = 3 |> [0,1,1]

v001 :: Vector Double
v001 = 3 |> [0,0,1]

v111 :: Vector Double
v111 = 3 |> [1,1,1]
p011 = map fromList $ nub $ permutations $ toList v011
p001 = map fromList $ nub $ permutations $ toList v001

cekEdge3 = isCorner

isMirrorSide i j = elem True $ map (t1 (abs $ i - j)) p001

isCorner i
  | (i == zeroVec) = False
  | otherwise = elem True $ map (t1 i) $ v111 : (p011 ++ p001)

t1 a b = (1 - norm_2 (a - b) )^2 < 0.000001

onlyMatrix m = unlines $ tail $ lines $ dispf 6 m
cekEdge2 :: Vector Double -> Vector Double -> Bool
cekEdge2 i j =
  let deltaV a b = (i - j)
      sameNum a b = (a - b)^2 < 0.00001
      p011 = map fromList $ nub $ permutations $ toList v011
      p001 = map fromList $ nub $ permutations $ toList v001
      t1 a b = (1 - norm_2 (a - b) )^2 < 0.000001
      testOne :: Vector Double -> Vector Double -> Bool
      testOne a b
        -- | (a == zeroVec) = elem True $ map (t1 b) $ p011 ++ p001
        | (a == zeroVec) = isCorner b
        | otherwise = True
        -- | otherwise = t1 a b
   in elem i $ p011 ++ p001

zeroVec :: Vector Double
zeroVec = 3 |> [0,0,0]

comb :: Int -> [a] -> [[a]]
comb m xs = combsBySize xs !! m
 where
   combsBySize = foldr f ([[]] : repeat [])
   f x next = zipWith (++) (map (map (x:)) ([]:next)) next

cekEdge i j = let [x,y] = map toList [i,j]
                  sameNum a b = (a - b)^2 < 0.00001
                  cekSame [0,0,0] [1,1,1] = 2
                  cekSame a b = (length $ filter (== True) $ zipWith sameNum a b)
               in case (cekSame x y) of
                    0 -> False
                    1 -> False
                    2 -> True
                    _ -> True


firstQuadrant v = let fQ x r = r && (0 <= x) && (x <= 1)
                  in  foldr fQ True $ toList v

elemWith :: (a -> b -> Bool) -> a -> [b] -> Bool
elemWith fun needle hay =
  let cekF _ _ _ [] = False
      cekF res f n (h:hs) = if (f n h) then True
                                       else cekF res f n hs
   in cekF False fun needle hay


sameVec (_,v) (_,r) = sV v r
sV v r = (norm_2 $ v - r) < 0.0000001
--cek1Quadrant v = foldr (\a b -> (a >= 0) && b) True $ toList v
cekFirstQuadrant v = foldr (\a b -> (0 <= a) && (a <= 1) && b) True $ toList v

cek1Quadrant _ = True

  {-
cek1Quadrant v = let cekQ _ [] = True
                     cekQ r (a:as) = if  (a <= 0) then False
                                                 else cekQ r as
                    in cekQ True $ toList v
-}

  {-
--genLat :: Monad m => ([Vector Double],ByteString)
--       -> ProcessT m (Int,ByteString) ([Vector Double],ByteString)

genLat :: Monad m => MachineT m (Is ([Vector Double], ByteString)) ([Vector Double], ByteString)
genLat = construct $ await >>= go
  where go cur@(l,v) |
          (length l < 3) = do
                            yield cur
                            next <- await
                            go $! func cur next
        func (a1,_) (i,b) = ((getVec b):a1,b)


genLat' :: Monad m => ([Vector Double],ByteString)
       -> ProcessT m (Int,ByteString) ([Vector Double],ByteString)
--genLat :: Monad m => ProcessT m a a
genLat' seed = construct $ go seed
  where
    go cur@(l,v)
      | (length l) < 3 = do
                            yield cur
                            next <- await
                            go $! func cur next
      | otherwise = yield (l,v)
    func (a1,_) (i,b) = ((getVec b):a1,b)

-}

genVec :: Monad m => ProcessT m BS.ByteString (Vector Double)
genVec = repeatedly $ do
  line <- await
  yield $ fromList $ map read $ words $ B8.unpack line

getVec :: ByteString -> Vector Double
getVec line =
  let xs = (|>) 3 $ map read $ words $ B8.unpack line
   in xs



skipHeader :: Monad m => Int -> ProcessT m a a
skipHeader n = dropping n

--lineNum :: Monad m => ProcessT m BS.ByteString (Int, BS.ByteString)

--lineSource :: MonadIO m => SourceT m ByteString
lineSource = ioSource isEOF BS.getLine

ioSource k f = construct go where
  go = do
    cond <- liftIO k
    unless cond $ do
      liftIO f >>= yield >> go

moveVec :: Monad m => Vector Double -> ProcessT m (Vector Double) (Vector Double)
moveVec v = repeatedly $ do
  x <- await
  yield $ x - v

