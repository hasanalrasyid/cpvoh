#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict #-}

module Main where

import           Options.Applicative
import Data.Semigroup ((<>))
import Data.List.Split (splitOn)
import Language.Fortran.Parser.Utils (readReal)
import Data.Maybe (fromJust)
import Linear.V3
import Linear.Vector ((*^))
import Text.Printf (printf)
import Data.List (isInfixOf,findIndex,intercalate)
import System.Directory (doesFileExist)
import Linear.Matrix -- inv33, M33

deg2rad deg = deg * pi / 180

v3fromList [a,b,c] = V3 a b c

main :: IO ()
main = do
    opts <- execParser withHelp
    bOK <- doesFileExist $ _inFort19CPVO opts
    if bOK then genPrimitive opts
           else do
             putStrLn "Error... input needed"
             putStrLn "genSurface.hs -i fort.19 -c celldm0 -s 2x2x1"

genPrimitive opts = do
  fFort19 <- readFile $ _inFort19CPVO opts
  fCellDM <- readFile $ _inCellDM0 opts
  let allSize@(s1:s2:s3:_)  = map (((flip (-)) 1) . floor . getReal)
                    $ splitOn "x" $ _inSize opts
  let (ibrav:cell_a:_) = map getReal $ words fCellDM
      fort19 = getAtomCoords [] $ lines fFort19
  let mLatVec = genLatticeVector ibrav [cell_a]
      target =[ (x,y,z) | x <- [0..s1]
                        , y <- [0..s2]
                        , z <- [0..s3]
              ]
  let h = concat $ map (reverse . genMirror target mLatVec [] ) fort19
  putStrLn $ unlines $ filter (not . null) $ concat $ map lines h
  let xnel = genXNEL 0 $ map words $ filter (isInfixOf "IS") h
  putStrLn $ "XNEL " ++ show xnel
  putStrLn $ show (floor $ xnel/2) ++ "*1.0"
  putStrLn $ "allSize = " ++ show (map (+1) allSize)
  let newCell_a = cell_a * fromIntegral (1 + maximum allSize)
  putStrLn $ "cell_a * maxSize = " ++ show newCell_a
  putStrLn $ unwords [show ibrav, show newCell_a, unwords $ drop 2 $ words fCellDM]

genXNEL res [] = res
genXNEL res ((_:sAts:sZV:_):ls) =
  genXNEL ((+) res $ foldr (*) 1.0 $ map getReal [sAts,sZV]) ls

showVec n (V3 a b c) = unwords $ map (printf (concat ["  %.",show n,"f"])) [a,b,c]

genMirror _ _ r [] = r
genMirror t m res (a:cs)
  | (isInfixOf "IS" a) = let
                          (noAt:_:sisa) = words a
                          numAts = (length t * length cs)
                          newA = unwords (noAt:show numAts:sisa)
                          in genMirror t m (newA:res) cs

  | otherwise = let
                  r' = map (showVec 6)
                     $ map (genMirrorFromSingle m (v3fromLine a)) t
                  ender = ("  " ++) $ unwords $ drop 3 $ words a
                  r  = unlines $ map (++ ender) r'
                 in genMirror t m (r:res) cs

genMirrorFromSingle :: M33 Double -> V3 Double -> (Integer,Integer,Integer)
                    -> V3 Double
genMirrorFromSingle m@(V3 v1 v2 v3) l all@(n1,n2,n3) =
  foldr (+) l $ zipWith calcMove [v1,v2,v3] [n1,n2,n3]
    where
      calcMove v' n' = fromIntegral n' *^ v'

getAtomCoords res [] = res
getAtomCoords res xa@(x:xs) =
  let (r1,rs) = case (findIndex (isInfixOf "IS") xs) of
                  Just a -> splitAt (a+1) xa
                  Nothing -> (xa,[])
  in getAtomCoords (r1:res) rs

genLatticeVector :: Double -> [Double] -> M33 Double
genLatticeVector 2 (a:_) = (a/2) *!! V3 (V3 0.0 1.0 1.0)
                                        (V3 1.0 0.0 1.0)
                                        (V3 1.0 1.0 0.0)

getReal s = case readReal s of
              Just x -> x
              _ -> 0

v3fromLine l = v3fromList $ map (fromJust . readReal) $ take 3 $ words l

data Opts = Opts {
    _inFort19CPVO :: FilePath,
    _inCellDM0 :: FilePath,
    _inSize :: String
                 } deriving Show

optsParser :: Parser Opts
optsParser = Opts
             <$> strOption (long "input-cpvo" <> short 'i' <> metavar "FORT19"
                            <> help "file input from fort.19 in CPVO run, usually came out after opts" <> value "fort.19")
             <*> strOption (long "input-celldm0" <> short 'c' <> metavar "CELLDM0"
                            <> help "file input from celldm0 in CPVO run, usually defined as an input" <> value "celldm0")
             <*> strOption (long "input-size" <> short 's' <> metavar "N1xN2xN3"
                            <> help "Expansion size of the supercell, should be integer: 2x2x1" <> value "2x2x1")

withHelp :: ParserInfo Opts
withHelp = info
       (helper <*> optsParser)
       (fullDesc <> progDesc "interpret inline Haskell code to insert images in Pandoc output\nhttps://github.com/bergey/diagrams-pandoc"
       <> header "diagrams-pandoc - a Pandoc filter for inline Diagrams")

