#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict #-}

module Main where

import           Options.Applicative
import Data.Semigroup


import Data.List.Split (splitOn)
import Language.Fortran.Parser.Utils (readReal)
import Data.Maybe (fromJust,fromMaybe)
import qualified Data.Map.Strict as M (fromList,lookup,Map)
import Data.Sequence (fromList,(|>),Seq,index)
import Data.Foldable (toList)
import Linear.Quaternion
import Linear.V3
import Linear.Metric (norm)
import Linear.Vector (zero, (*^),(^/),(^+^),(^-^),(*^))
import Text.Printf (printf)
import Linear.V as V
import Data.Char (isAlpha,toUpper)
import Data.List (groupBy)
import System.Directory (doesFileExist)
-- linear-accelerate 0.6.0.0
-- inv33 ternyata cuma ada di Data.Array.Accelerate.Linear.Matrix
--import qualified Data.Array.Accelerate as A -- lift
--import Data.Array.Accelerate.Linear.Matrix (inv33) -- inv33
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
             putStrLn "genSurface.hs -i fort.19 -c celldm0"


genPrimitive opts = do
  fFort19 <- readFile $ _inFort19CPVO opts
  print fFort19
    {-
  let sMir_Ref = _newBasisVectors opts
  let (headerfFort19,coordfFort19) = splitAt 8 $ lines fFort19
  let a@[vAConv,vBConv,vCConv] = map v3fromLine $ drop 2 $ take 5 $ headerfFort19
      matConv2Origin = transpose $ V3 vAConv vBConv vCConv :: M33 Double
      matOrigin2Conv = inv33 matConv2Origin

      coordVCart = map v3fromLine coordfFort19
      vRef = (V3 0.00000  0.00000  0.14523 ) :: V3 Double
      vAMir = (V3 0.33333  0.66667  0.52144) :: V3 Double
      vBMir = (V3 0.33333 (-0.33333)  0.52144) :: V3 Double
      vCMir = (V3 (-0.66667) (-0.33333)  0.52144) :: V3 Double
      vAFracPrim = vAMir ^-^ vRef
      vPrim = map (getPrim vAConv vBConv vCConv)
            $ map ((flip (^-^)) vRef) [vAMir,vBMir,vCMir]
  putStrLn $ unlines $ map show coordVCart
  putStrLn $ unlines $ map (showVec 4) vPrim
  let coordVConv = map (matOrigin2Conv !*) coordVCart
      iRef = 7
      sA   = "12 -  7"
      sB   = "12 - 10"     -- vB = 12 - 10
      sC   = " 8 - 12"     -- vC =  8 - 12
      vRef = coordVConv
  let coordVCart' = map (matConv2Origin !*) coordVConv
  putStrLn $ show matConv2Origin
  putStrLn $ show $ head coordVCart
  putStrLn $ unlines $ map show $ zip [1,2..] $ map (showVec 6) $ coordVCart ^-^ coordVCart'  -- dari sini transformasinya sudah benar tampaknya....
  putStrLn $ unlines $ map show $ zip [1,2..] $ map (showVec 6) $ coordVConv
  -}

v3fromLine l = v3fromList $ map (fromJust . readReal) $ words l

getPrim :: V3 Double -> V3 Double -> V3 Double -> V3 Double -> V3 Double
getPrim vAConv vBConv vCConv (V3 xiF yiF ziF) =
  foldr (+) zero $ zipWith (*^) [xiF, yiF, ziF] [vAConv, vBConv, vCConv]


{-
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
-}

mkHeaders :: [(String,Int)] -> [[String]] -> [(String,String)]
mkHeaders nas lhs = let newLhs = map (\(s:nAt:_:ss) -> (s,(nAt,unwords ss))) lhs
                        mkHead (x,nx) = (,) x $ case (lookup x newLhs) of
                                             Just (n,a) -> unwords [ n, show nx, a]
                                             Nothing -> unwords ["ERROR: HEADER FOR ATOM ", show x , " IS NOT DEFINED"]
                                          in map mkHead nas

showVec :: Integer -> V3 Double -> String
showVec n (V3 a b c) = unwords $ map (printf (concat ["%.",show n,"f"])) [a,b,c]

showCart (nm,vec) = unwords $ [nm,  showVec 6 vec]

upCase s = map toUpper s

xyzFormat l c = (show l : "DummyTitle" : map showCart c)
cpvoFormat atHeaders x = concat $ map (\x -> concat [[fromJust $ lookup (upCase $ fst $ head x) atHeaders] , map snd x])
  $ groupBy (\(a,_) (b,_) -> a == b)
  $ map (\(s,v) -> (takeWhile isAlpha s, showVec 6 v)) x

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

data Opts = Opts {
    _inFort19CPVO :: FilePath,
    _inCellDM0 :: FilePath
                 } deriving Show

optsParser :: Parser Opts
optsParser = Opts
             <$> strOption (long "input-cpvo" <> short 'i' <> metavar "FORT19"
                            <> help "file input from fort.19 in CPVO run, usually came out after opts" <> value "fort.19")
             <*> strOption (long "input-celldm0" <> short 'c' <> metavar "CELLDM0"
                            <> help "file input from celldm0 in CPVO run, usually defined as an input" <> value "celldm0")

withHelp :: ParserInfo Opts
withHelp = info
       (helper <*> optsParser)
       (fullDesc <> progDesc "interpret inline Haskell code to insert images in Pandoc output\nhttps://github.com/bergey/diagrams-pandoc"
       <> header "diagrams-pandoc - a Pandoc filter for inline Diagrams")

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


