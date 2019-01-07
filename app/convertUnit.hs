#!/usr/bin/env stack
--stack --resolver lts-2.22 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml


{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE Strict #-} -- not yet available in GHC 7
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Options.Applicative as O
-- import Data.Semigroup
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
import CPVO.Parser
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

  {-
mainOld = do
  opts <- execParser withHelp
  let interPoints = read $ _interPoints opts :: Integer
      initDir = _initDir opts
  putStrLn $ "======START: TS finding====="
  putStrLn $ "==Reading initial position"
  (fileHeader,fileTailer,initCrystal) <- readCrystal initDir
  putStrLn $ "==Reading last position"
  (_,_,lastCrystal) <- readCrystal $ _lastDir opts
  let err = structureSize (lastCrystal - initCrystal) < 0.0001
  case err of
    True -> putStrLn "Error: no difference in initial and final structure"
    _ -> do
      putStrLn $ "==Generating " ++ show interPoints ++ " intermediate positions"
      genIntermediate (1+interPoints) initDir fileHeader fileTailer initCrystal lastCrystal
      putStrLn $ "==== Preparing cpvo_sysp.sh==="
      putStrLn $ "==== spawning runner.sh"
      jcfHandle <- spawnCommand "./runner.sh"
      putStrLn $ "==== orchestrating for IRC"
      runIRC jcfHandle Nothing
-}

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

instance Show [Coord] where
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
  {-
deg2rad deg = deg * pi / 180

v3fromList [a,b,c] = V3 a b c


processZMatrix opts = do
    let vTranslation = v3fromList $ map (fromJust . readReal) $ words $ _translationVector opts
        aRotRad = deg2rad $ fromJust $ readReal $ _rotationAngle opts
        vRot = vUnity $ v3fromList $ map (fromJust . readReal) $ words $ _rotationAxis opts
    z <- getContents
    let vInit0 = (V3 0 0 0) :: V3 Double
        dInit@(dInitDih,vInit1) =  (180, (V3 1 0 0)) :: (Double,V3 Double)
        (struct:vars:_) = map (filter (not . null)) $ splitOn [["Variables:"]] $ drop 5 $ map words $ lines z
        varMap = M.fromList $ map (\[a,b] -> (a, fromJust $ readReal b)) vars
    let cart0 = genCart varMap vInit0 dInit struct $ fromList []
        nAtoms = map (\x -> (fst $ head x, length x)) $ groupBy (\x y -> fst x == fst y)
                 $ map (\(s,v) -> (upCase $ takeWhile isAlpha s,v)) $ toList cart0
        atomicHeaders = mkHeaders nAtoms $ map words $ lines $ _cpvoAtHeads opts
    ---------------------------PRINTING-----------------------------
    let chosenFormat = if (_cpvoFormat opts) then cpvoFormat atomicHeaders else xyzFormat (length struct)
    putStrLn $ unlines
      $ chosenFormat
      $ map (\(s,v) -> (s, (^+^) vTranslation $ (flip rotate) v $ axisAngle vRot aRotRad))
      $ toList cart0

mkHeaders :: [(String,Int)] -> [[String]] -> [(String,String)]
mkHeaders nas lhs = let newLhs = map (\(s:nAt:_:ss) -> (s,(nAt,unwords ss))) lhs
                        mkHead (x,nx) = (,) x $ case (lookup x newLhs) of
                                             Just (n,a) -> unwords [ n, show nx, a]
                                             Nothing -> unwords ["ERROR: HEADER FOR ATOM ", show x , " IS NOT DEFINED"]
                                          in map mkHead nas

showVec :: V3 Double -> String
showVec (V3 a b c) = unwords $ map (printf "%.6f") [a,b,c]

showCart (nm,vec) = unwords $ [nm,  showVec vec]

upCase s = map toUpper s

xyzFormat l c = (show l : "DummyTitle" : map showCart c)
cpvoFormat atHeaders x = concat $ map (\x -> concat [[fromJust $ lookup (upCase $ fst $ head x) atHeaders] , map snd x])
  $ groupBy (\(a,_) (b,_) -> a == b)
  $ map (\(s,v) -> (takeWhile isAlpha s, showVec v)) x

genCart :: M.Map String Double -> V3 Double -> (Double,V3 Double) -> [[String]] -> Seq (String, V3 Double) -> Seq (String, V3 Double)
genCart _ _ _ [] res = res
genCart vMap v0 d1@(dih,v1) (x:xs) res = genCart vMap v0 d1 xs $ res |> fromZMat x
  where
    fromZMat :: [String] -> (String, V3 Double)
    fromZMat [s1] = (s1,v0)  -- C1
    fromZMat [s2a,_,r2] = (s2a, (callVar r2) *^ (vUnity v1))  -- C2
    fromZMat [s3a,r3ref,r3,a3ref,a3] =
      let ca3 = callVarAngle a3   -- C3
          cr3 = callVar r3
          [v_i,v_j] =
            map (snd . (index res) . floor . (+ (-1)) . fromJust . readReal)
              [r3ref,a3ref]  :: [V3 Double]
          v_ij = v_j ^-^ v_i
          vV = (*^) cr3 $ vUnity $ v_ij
          vrQ = v_i ^+^ (rotateQ (V3 0 0 1) ca3 vV)
       in (s3a, vrQ)
    fromZMat (sNa:rNref:rNs:aNref:aNs:dNref:dNs:_) = -- C4
      let rtp@[rni,teta,phi] = zipWith ($) [callVar,callVarAngle,callVar] [rNs,aNs,dNs]
          [va_i,va_j,va_k] =
            map (snd . (index res) . floor . (+ (-1)) . fromJust . readReal)
              [rNref,aNref,dNref]  :: [V3 Double]
          v_ij = va_j ^-^ va_i
          v_ik = va_k ^-^ va_i
          vn_ijk = vUnity $ cross v_ij v_ik
          q2 = axisAngle v_ij $ deg2rad phi
          q1 = axisAngle vn_ijk $ deg2rad teta
          totQ = q2 * q1 -- we do q1 first, then q2
          vrQ = (*^) rni $ vUnity $ rotate totQ v_ij
      in (sNa, va_i ^+^ vrQ)
    callVar :: String -> Double
    callVar s = fromJust $ M.lookup s vMap
    callVarAngle s = abs $ (flip remD) 180 $ callVar s

vUnity v = v ^/ (norm v)
remD a b = (fromIntegral $ rem (floor a) (floor b))


rotateQ axis thetaDeg v = rotate q v
  where
    q = axisAngle axis $ deg2rad thetaDeg

defAtHeaders = [ "H   1 1  1.00 0.800   1.000   0.80  0.80  NZA NA ZV RCMAX PMASS RATS RATS1"
               , "C   6 4  4.0  1.0    12.0000  1.33  1.33  NZA NA ZV RCMAX PMASS RATS RATS1"
               , "N   7 2  5.0  1.0    14.007   1.3   1.3   NZA NA ZV RCMAX PMASS RATS RATS1"
               , "O   8 6  6.00 1.000  15.999   1.3   1.3   NZA NA ZV RCMAX PMASS RATS RATS1"
               , "Mg 12 6  8.00 1.000  24.305   1.7   1.7   NZA NA ZV RCMAX PMASS RATS RATS1"
               , "Si 14 8  4.00 1.0    28.086   1.75  1.75  NZA NA ZV RCMAX PMASS RATS RATS1"
               , "Ar 18 1  8.00 1.000  39.948   1.4   1.4   NZA NA ZV RCMAX PMASS RATS RATS1"
               , "Co 27 2 17.0  1.0    58.9332  2.2   2.2   NZA NA ZV RCMAX PMASS RATS RATS1"
               , "Sb 51 2 15.00 1.000 121.75000 2.500 2.500 NZA NA ZV RCMAX PMASS RATS RATS1"
               , "Ba 56 7 10.00 1.000 137.327   2.6   2.6   NZA NA ZV RCMAX PMASS RATS RATS1"
               , "Pt 78 1 10.0  1.0   195.09    2.32  2.32  NZA NA ZV RCMAX PMASS RATS RATS1"
               , "Au 79 1 11.0  1.0   196.9665  2.7   2.7   NZA NA ZV RCMAX PMASS RATS RATS1"
               , "Ti 81 2 13.0  1.0   204.37    2.5   2.5   NZA NA ZV RCMAX PMASS RATS RATS1"
               , "Pb 82 4 14.0  1.0   207.2     2.5   2.5   NZA NA ZV RCMAX PMASS RATS RATS1"
               ]

-}


--phi_mu vr_i = foldl (+) (\(d_pmu,alpha_pmu) -> d_pmu * (sqrt ( sqrt ((2 * alpha_pmu / pi)^3))) * (exp $ (-1) * alpha_pmu * (norm_2 (vr_i - vR_i))^2)) basisset

  {-
psi = sum $ zipWith (*) phi $ coefC_mui

sto :: ExponentCoef -> ContractionCoef -> Cart -> Basis
sto a d vR = d * exp ((-1) * a * (norm_2 vR)^2)

phi basis coefSet_mu vR_mu vr =  let dCoef = d * ((sqrt . sqrt) $ (2 * a / pi)^3)
                                     primitiveBasis a d = basis a dCoef (vr - vR_mu)
                                 in sum $ map (\(aC,dC) -> primitiveBasis aC dC) coefSet_mu

twoElIntegralS_munu basis basisset_mu basisset_nu vR_mu vR_nu =
  let primitiveBasis a1 d1 a2 d2 = basis a_munu d_munu vR_munu
      a_munu = a1 * a2/(a1pa2)
      d_munu = d1 * d2 * sqrt $ (pi / (a1pa2))^3
      a1pa2 = a1 + a2
  in sum $ map (\((a_mu,d_mu),(a_nu,d_nu)) -> primitiveBasis a_mu d_mu a_nu d_nu) $ multMOp basisset_mu basisset_nu

coreHamiltonianH_munu zA = oneElectronKineticT_munu + potentialEnergyIntegralV zA

oneElectronKineticT basisset_mu vR_mu basisset_nu vR_nu = sum $ map (\((a_mu,d_mu),(a_nu,d_nu)) -> primitiveBasis a_mu d_mu a_nu d_nu) $ multMOp basisset_mu basisset_nu
  where
    a1pa2 = a1 + a2
    a_munu = a1 * a2/(a1pa2)
    vR_munu = vR_mu - vR_nu
    d_munu = d1 * d2 * a_munu * (3 - 2 * a_munu * ((norm_2 vR_munu)^2)) * sqrt $ pi /a1pa2
    primitiveBasis a1 d1 a2 d2 = basis a_munu d_munu vR_munu

potentialEnergyIntegralV zA basisset_mu vR_mu basisset_nu vR_nu = sum $ map (\(z,(a_mu,d_mu),(a_nu,d_nu)) -> primitiveBasis z a_mu d_mu a_nu d_nu) $  multMOp3 zA basisset_mu basisset_nu
  where
    a1pa2 = a1 + a2
    a_munu = a1 * a2/(a1pa2)
    vR_munu = vR_mu - vR_nu
    d_munu = d1 * d2 * pi * (z / a1pa2) * sqrt $ pi / (a1pa2 * (norm_2 vR_munu)^2)
    primitiveBasis z a1 d1 a2 d2 = (erf $ R_murho * sqrt a1pa2 ) * basis a_munu d_munu vR_munu

twoElectronIntegralG (bsset_mu,bsset_nu,vR_mu, vR_nu) (bsset_sigma,bsset_lambda,vR_s,vR_l) =
  sum $ map (\((a_m,d_m),(a_n,d_n),(a_s,d_s),(a_l,d_l)) -> primitiveBasis a_m d_m a_n d_n a_s d_s a_l d_l )
    $ [(m,n,s,l) | m <- bsset_mu, n <- bsset_nu, s <- bsset_sigma, l <- bsset_lambda]
  where
    vR_munu = vR_mu - vR_nu
    vR_rhot = vR_rho - vR_t
    vR_rho = undefined
    vR_t =  scale (a3 + a4) $ (scale a3 vR_s) + (scale a4 vR_l)
    vR_sl = vR_s - vR_l
    vR_munu = vR_mu - vR_nu
    a3pa4 = a3 + a4
    a_sl = a3 * a4/a3pa4
    d_sl = 1
    a1pa2 = a1 + a2
    a_munu = a1 * a2/a1pa2
    d_munu = d1 * d2 * d3 * d4 * (2 * pi * pi * sqrt pi )/((a1pa2)*(a3pa4)*sqrt(a1pa2 + a3pa4)) * vR_rhot * sqrt (pi * (a1pa2 + a3pa4)/(a1pa2 * a3pa4))
    primitiveBasis a1 d1 a2 d2 a3 d3 a4 d4 = (erf $ vR_rhot * sqrt $ (a1 + a2) * (a3 + a4) / (a1 + a2 + a3 + a4) ) * (basis a_munu d_munu vR_munu) * (basis a_sl d_sl vR_sl)

multMOp a b = [(i,j)| i <- a, j <- b]
multMOp3 h a b = [(k,i,j)| k <- h, i <- a, j <- b]


-}

