#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/hascpvo/stack.yaml

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict #-}

module Main where

import           Options.Applicative
import Data.Semigroup

import Data.List.Split (splitOn)
import Language.Fortran.Parser.Utils (readReal)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M (fromList,lookup,Map)
import Data.Sequence (fromList,(|>),Seq,index)
import Data.Foldable (toList)
import Linear.Quaternion
import Linear.V3
import Linear.Metric (norm)
import Linear.Vector
import Text.Printf (printf)
import Linear.V as V
import qualified Data.IntMap as IM

deg2rad deg = deg * pi / 180

v3fromList [a,b,c] = V3 a b c

main :: IO ()
main = do
    opts <- execParser withHelp
    putStrLn $ show opts
    let vTranslation = v3fromList $ map (fromJust . readReal) $ words $ _translationVector opts
        aRotRad = deg2rad $ fromJust $ readReal $ _rotationAngle opts
        vRot = vUnity $ v3fromList $ map (fromJust . readReal) $ words $ _rotationAxis opts
    z <- getContents
    let vInit0 = (V3 0 0 0) :: V3 Double
        dInit@(dInitDih,vInit1) =  (180, (V3 1 0 0)) :: (Double,V3 Double)
        (struct:vars:_) = map (filter (not . null)) $ splitOn [["Variables:"]] $ drop 5 $ map words $ lines z
        varMap = M.fromList $ map (\[a,b] -> (a, fromJust $ readReal b)) vars
    let cart0 = genCart varMap vInit0 dInit struct $ fromList []
    ---------------------------PRINTING-----------------------------
    putStrLn $ show $ length struct
    putStrLn "DummyTitle"
    putStrLn $ unlines $ map showCart
      $ map (\(s,v) -> (s, (^+^) vTranslation $ (flip rotate) v $ axisAngle vRot aRotRad))
      $ toList cart0

showVec :: V3 Double -> String
showVec (V3 a b c) = unwords $ map (printf "%.6f") [a,b,c]

showCart (nm,vec) = unwords $ [nm,  showVec vec]

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
    _outFormat    :: String,
    _outDir       :: FilePath,
    _expression   :: String,
    _absolutePath :: Bool,
    _rotationAngle :: String,
    _rotationAxis :: String,
    _cpvoFormat :: Bool,
    _translationVector :: String
                 } deriving Show

optsParser :: Parser Opts
optsParser = Opts
             <$> strArgument (help "target output format from pandoc" <> value "html")
             <*> strOption (long "out" <> short 'o' <> metavar "DIR"
                            <> help "Directory for image files" <> value "images")
             <*> strOption (long "expression" <> long "expr" <> short 'e' <>
                            metavar "NAME" <>
                            help "name of Diagram value in Haskell snippet" <>
                            value "example")
             <*> switch    (long "absolute" <> short 'z' <>
                            help "output the name of Diagram in Haskell snippet as absolute path")
             <*> strOption (long "rotation-angle" <> short 'a' <> metavar "DEGREE"
                            <> help "rotational angle, in Degree, the default would be \"0\", if negative, put it inside double-tick, i.e. \"-32\"" <> value "0")
             <*> strOption (long "rotation-axis" <> short 'r' <> metavar "\"x y z\""
                            <> help "i j k vector for rotational axis, default would be z-axis, 0 0 1" <> value "0 0 1")
             <*> switch    (long "cpvo" <> short 'c' <>
               help "output in CPVO format. Due to different preference in RATS, please provide your own header using -i. defaulted to False.")
             <*> strOption (long "translation-vector" <> short 't' <> metavar "\"x y z\""
                            <> help "i j k vector/coordinate to move the system, defaulted to 0 0 0" <> value "0 0 0")

withHelp :: ParserInfo Opts
withHelp = info
       (helper <*> optsParser)
       (fullDesc <> progDesc "interpret inline Haskell code to insert images in Pandoc output\nhttps://github.com/bergey/diagrams-pandoc"
       <> header "diagrams-pandoc - a Pandoc filter for inline Diagrams")
