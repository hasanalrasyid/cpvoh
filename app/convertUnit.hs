#!/usr/bin/env stack
--stack --resolver lts-14.27 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml


{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE Strict #-} -- not yet available in GHC 7
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Options.Applicative as O
import Data.Semigroup ((<>))
--
-- import Data.List.Split (splitOn)
-- import Language.Fortran.Parser.Utils (readReal)
-- import Data.Maybe (fromJust,fromMaybe)
-- import qualified Data.Map.Strict as M (fromList,lookup,Map)
-- import Data.Sequence (fromList,(|>),Seq,index)
-- import Data.Foldable (toList)
-- import Linear.Quaternion
-- import Linear.V3
-- import Linear.Metric (norm)
-- import Linear.Vector
-- import Text.Printf (printf)
-- import Linear.V as V
-- import Data.Char (isAlpha,toUpper)
-- import Data.List (groupBy)

import CPVO.IO
import CPVO.Data
import CPVO.IO.Reader.Parser
--import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.HMatrix hiding ((<>))
-- import System.Environment (getArgs)
import Data.Attoparsec.Text hiding (takeWhile)
import qualified Data.Text.IO as T (readFile)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import System.Process  -- getProcessExitCode system spawnCommand

main :: IO ()
main = do
  opts <- execParser withHelp
  angstromFile <- T.readFile $ _inFile opts
  let fConv = readConvert $ _toUnit opts
  let res = map (map (convert fConv) . T.words) $ T.lines angstromFile
  putStrLn $ unlines $ map unwords res

readConvert "bohr" = toBohr
readConvert "angs" = toAngstrom
readConvert _ = id

toAngstrom b = b * 0.529177
toBohr a = a * 1.88973

convert f t = case T.double t of
                 Right (a,_) -> case (showDouble 8 $ f a) of
                                  "0" -> "0.0"
                                  a   -> a
                 Left _ -> T.unpack t

runIRC jcfHandle (Just _) = do
  putStrLn $ "=== EXIT: runner.sh"
runIRC jcfHandle Nothing = do
  jcfS <- getProcessExitCode jcfHandle
  putStrLn $ "==== Status of JCF: " ++ show jcfS
  putStrLn $ "==== checking Force and everything else"
  _ <- system "sleep 2s"
  runIRC jcfHandle jcfS


genIntermediate :: Integer -> String -> T.Text -> T.Text -> Crystal -> Crystal -> IO ()
genIntermediate numInter initDir fileHeader fileTailer initCrystal lastCrystal = do
  mapM_ (genSingleCrystal numInter initDir fileHeader fileTailer initCrystal lastCrystal) [1..(numInter - 1)]

structureSize :: Crystal -> Double
--length (Crystal _ _ _ _ as) = norm_Frob $ fromRows $ map (fromCart . toCart) $ concat $ map positions as
structureSize (Crystal _ _ _ _ as) = norm_Frob $ fromRows $ map (fromCart . toCart) $ concat $ map positions as
structureSize ErrCrystal = (-1)


genSingleCrystal :: Integer -> String -> T.Text -> T.Text -> Crystal -> Crystal -> Integer -> IO ()
genSingleCrystal numInter initDir fileHeader fileTailer initCrystal lastCrystal numPart = do
  let candidateDir = "candidate_" ++ show numPart
  inshell2text $ unwords ["rm -rf",candidateDir,"; cp -a",initDir,candidateDir]
  let newAtomList = zipWith (genAtomList numPart numInter) (atomList initCrystal) $ atomList lastCrystal
      newCrystal = initCrystal { atomList = newAtomList }
  writeFile (candidateDir ++ "/sample.dat.sh") $ T.unpack $ T.concat [ fileHeader,T.concat[(T.pack $ show newCrystal),fileTailer]]
  _ <- inshell2text $ "chmod +x " ++ candidateDir ++ "/*.sh"
  putStrLn $ "====Finished generating candidate : " ++ show numPart

genAtomList :: Integer -> Integer -> Atoms -> Atoms -> Atoms
genAtomList _ _ ErrAtoms _ = ErrAtoms
genAtomList _ _ _ ErrAtoms = ErrAtoms
genAtomList numPart numInter (Atoms s pos0) (Atoms _ posT) =
--  let genPos (Atoms ) (_,Cart pT) = (t,Cart (p0 + (pT-p0) * fromIntegral numPart/fromIntegral numInter))
  let genPos ErrCoord _ = ErrCoord
      genPos _ ErrCoord = ErrCoord
      genPos  (Coord t (Cart p0)) (Coord _ (Cart pT)) =
        Coord t $ Cart (p0 + (pT-p0) * fromIntegral numPart/fromIntegral numInter)
   in Atoms s $ zipWith genPos pos0 posT

readCrystal :: String -> IO (T.Text, T.Text, Crystal)
readCrystal workDir  = do
  fCellDm <- T.readFile $ workDir ++ "/celldm0"
  fCart''  <- T.readFile $ workDir ++ "/sample.dat.sh"
  let Right (ibrav, celldms) = parseOnly parseCellParam fCellDm
      (atHead,a') = break (T.isInfixOf "NZA") $ T.lines fCart''
      (atInput,atTail) = break (T.isInfixOf "end") a'
  let atoms  = parseOnly parseSample $ T.unlines atInput
  return $ (,,) (T.unlines atHead) (T.unlines atTail) $ case atoms of
                  Right a -> Crystal ibrav celldms (matrix 1 [1])  (matrix 1 [1]) a
                  _ -> ErrCrystal

instance Num Crystal where
  (-) _ ErrCrystal = ErrCrystal
  (-) ErrCrystal _ = ErrCrystal
  (-) (Crystal a b c d as) (Crystal _ _ _ _ bs) =
    Crystal a b c d $ zipWith (-) as bs
  (+) _ ErrCrystal = ErrCrystal
  (+) ErrCrystal _ = ErrCrystal
  (+) (Crystal a b c d as) (Crystal _ _ _ _ bs) =
    Crystal a b c d $ zipWith (+) as bs
  (*) _ _ = ErrCrystal
  abs = id
  signum = id
  fromInteger _ = ErrCrystal

instance Num Atoms where
  (-) ErrAtoms _ = ErrAtoms
  (-) _ ErrAtoms = ErrAtoms
  (-) (Atoms a p1) (Atoms _ p2) = Atoms a $ zipWith (-) p1 p2
  (+) ErrAtoms _ = ErrAtoms
  (+) _ ErrAtoms = ErrAtoms
  (+) (Atoms a p1) (Atoms _ p2) = Atoms a $ zipWith (+) p1 p2
  (*) _ _ = ErrAtoms
  abs = id
  signum = id
  fromInteger _ = ErrAtoms

instance Num Coord where
  (-) ErrCoord _ = ErrCoord
  (-) _ ErrCoord = ErrCoord
  (-) (Coord t (Cart a)) (Coord _ (Cart b)) = Coord t $ Cart (a - b)
  (+) ErrCoord _ = ErrCoord
  (+) _ ErrCoord = ErrCoord
  (+) (Coord t (Cart a)) (Coord _ (Cart b)) = Coord t $ Cart (a + b)
  (*) _ _ = ErrCoord
  fromInteger _ = ErrCoord
  abs = id
  signum = id

--  let genPos (t,Cart p0) (_,Cart pT) = (t,Cart (p0 + (pT-p0) * fromIntegral numPart/fromIntegral numInter))

instance Show Crystal where
  show ErrCrystal = "ERROR: NO CRYSTAL FOUND"
  show (Crystal _ _ _ _ ats) = concat $ map showSingleAt ats
    where
      showSingleAt a = concat [show (atomSpec a), show (positions a)]

instance Show Atom where
  show ZeroAtom = "UNKNOWN ATOM = Atom ZeroAtom"
  show (Atom a b c d e f g) =
    let h1 = unwords $ (:) (show a)
                             $ (:) (show b)
                             $ map show [c,d,e,f,g]
     in h1 ++ " NZA NA ZV RCMAX PMASS RATS RATS1" ++ newLine

newLine :: String
newLine = unlines [""]

instance Show Cart where
  show c = unwords $ map (showDouble 8) $ toList $ fromCart c

instance Show Coord where
  show ErrCoord = "UNKNOWN COORD: ErrCoord"
  show (Coord a c) = show c ++ T.unpack a

instance {-# OVERLAPS #-} Show [Coord] where
  show a = unlines $ map show a

data Opts = Opts {
    _inFile :: FilePath,
    _jcfFile :: FilePath,
    _toUnit :: String
                 } deriving Show

optsParser :: O.Parser Opts
optsParser = Opts
             <$> strOption (long "inFile" <> short 'i' <> metavar "INPUTFILE"
                            <> help "Input file, in Angstrom, to be converted to AU Bohr" <> value "initfile.xyz")
             <*> strOption (long "jcf" <> short 'j' <> metavar "JCF"
                            <> help "Runner for CPVO, i.e. qsub jcf.sekirei.sh" <> value "runner.sh")
             <*> strOption (long "toUnit" <> short 't' <> metavar "UNIT"
                            <> help "Unit of conversion, bohr or angs (Angstrom)" <> value "bohr")

withHelp :: ParserInfo Opts
withHelp = info
       (helper <*> optsParser)
       (fullDesc <> progDesc "Driving chain of states to create Internal Reaction Coordinates for CPVO program"
       <> header "Internal Reaction Coordinates for CPVO [IRC-CPVO]")

