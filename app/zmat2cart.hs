#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/hascpvo/stack.yaml

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict #-}

module Main where

import           Options.Applicative
import           Text.Pandoc.JSON
import Data.Semigroup

import System.IO (getContents)
import Data.List.Split (splitOn)
import Language.Fortran.Parser.Utils (readReal)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M (fromList,lookup,Map)
import Data.Tree (Tree(Node),drawTree)
import Data.Sequence (fromList,(|>),Seq,index)
import Data.Foldable (toList)
import qualified Numeric.LinearAlgebra as HM (norm_2,scale,cross,toList,dot)
import qualified Numeric.LinearAlgebra.Data as HM (fromList,fromRows,Vector,disps)
import Linear.Quaternion
import Linear.V3
import Linear.Metric (norm)
import Linear.Vector

deg2rad deg = deg * pi / 180

main :: IO ()
main = do
--    opts <- execParser withHelp
    z <- getContents
    let vInit0 = (V3 0 0 0) :: V3 Double
    --let vInit0 = HM.fromList [0,0,0] :: V3 Double
    let dInit@(dInitDih,vInit1) =  (180, (V3 1 0 0)) :: (Double,V3 Double)
--    putStrLn $ show opts
    let (struct:vars:_) = map (filter (not . null)) $ splitOn [["Variables:"]] $ drop 5 $ map words $ lines z
        varMap = M.fromList $ map (\[a,b] -> (a, fromJust $ readReal b)) vars
    putStrLn $ show $ length struct
    putStrLn "Judul---"
--    putStrLn $ show $ M.lookup "R6" varMap
    putStrLn $ unlines $ map showCart $ toList $ genCart varMap vInit0 dInit struct $ fromList []
--    putStrLn $ show $ genCartG varMap vInit0 dInit struct
    -- putStrLn $ show $ genCart [0,0,0] [0,0,0] struct $ fromList []
--    putStrLn $ drawTree $ fmap show (Node 1 [Node 2 [], Node 3 []])

showVec :: V3 Double -> String
showVec (V3 a b c) = unwords $ map show [a,b,c]


showCart (nm,vec) = unwords $ [nm,  showVec vec]

{-
type RefR a = ZCoord a
type RefA a = ZCoord a
type RefD a = ZCoord a

data ZCoord a = Origin0 a
                   | Origin1 a Length
                   | Origin2 a (RefR a) Length (RefA a) Angle
                   | NodeZ a (RefR a) Length (RefA a) Angle (RefD a) Dihedral
                   deriving (Show, Eq)


genCartG :: M.Map String Double -> V3 Double -> (Double,V3 Double) -> [[String]] -> ZCoord String
--genCartG _ _ _ [] res = res
--genCartG vMap v0 d1@(dih,v1) (x:xs) res = genCartG vMap v0 d1 xs $ insert res $ newNode x
genCartG vMap v0 d1@(dih,v1) (x:xs) =  NodeZ "n1" ref1r 1.1 ref1a 120.0 ref1d 120.0
  where ref1r = (Origin2 "or2"(Origin0 "or0") 1.5 (Origin1 "or1" 1.1) 120.0 )
        ref1a = (Origin1 "or1" 1.1)
        ref1d = (Origin0 "or0")

ZCoord = Tuple String (V3 Double) deriving Show

showZCoord (ZCoord (nm,vec)) = nm ++ "===" ++ (show vec)
-}

genCart :: M.Map String Double -> V3 Double -> (Double,V3 Double) -> [[String]] -> Seq (String, V3 Double) -> Seq (String, V3 Double)
genCart _ _ _ [] res = res
genCart vMap v0 d1@(dih,v1) (x:xs) res = genCart vMap v0 d1 xs $ res |> fromZMat x
  where
    fromZMat :: [String] -> (String, V3 Double)
    fromZMat [s1] = (s1,v0)  -- C1
    fromZMat [s2a,_,r2] = (s2a, (callVarR r2) *^ (vUnity v1))  -- C2
    fromZMat [s3a,r3ref,r3,a3ref,a3] =
      let ca3 = callVarAngle a3   -- C3
          cr3 = callVarR r3
          [v_i,v_j] =
            map (snd . (index res) . floor . (+ (-1)) . fromJust . readReal)
              [r3ref,a3ref]  :: [V3 Double]
          v_ij = v_j ^-^ v_i
          vV = (*^) cr3 $ vUnity $ v_ij
          vrQ = v_i ^+^ (rotateQ (V3 0 0 1) ca3 vV)
       in (s3a, vrQ)
    fromZMat (sNa:rNref:rNs:aNref:aNs:dNref:dNs:_) = -- C4
      let rtp@[rni,teta,phi] = zipWith ($) [callVarR,callVarAngle,callVarDih] [rNs,aNs,dNs]
          [va_i,va_j,va_k] =
            map (snd . (index res) . floor . (+ (-1)) . fromJust . readReal)
              [rNref,aNref,dNref]  :: [V3 Double]
          vAxis = va_j ^-^ va_i
          vV = va_k ^-^ va_i
          v_jk = va_k ^-^ va_j
          v_ij = va_j ^-^ va_i
          vn_ijk = vUnity $ cross vAxis v_jk
          vA = (*^) rni $ rotateQ vn_ijk teta $ vAxis
          vrQ = va_i ^+^ (rotateQ vAxis phi vA)
           in (sNa,vA)
{-
          vb_ik  = vUnity $ va_k - va_i
          vb_ij  = vUnity $ va_j - va_i
          vb_ijk = HM.scale  (1/(sin teta)) $ HM.cross vb_ij vb_ik
          vr_n   = (+) va_i
                 $ HM.scale rni $ foldl (+) (HM.fromList [0,0,0])  [ HM.scale (cos teta) vb_ij
                                            , HM.scale ((sin teta) * (cos phi)) (HM.cross vb_ijk vb_ij)
                                            , HM.scale ((sin teta) * (sin phi) * (-1)) vb_ijk
                                            ]
          vV = va_k - va_j
          vAxis = vUnity $ va_j - va_i  -- axis of rotation
          vr_n_rodriguez = vV + foldl (+) (HM.fromList [0,0,0]) [ HM.scale (cos phi) vV
                                                           , HM.scale (sin phi) $ HM.cross vR vV
                                                           , HM.scale ((1 - (cos phi)) * (HM.dot vR vV)) vR
                                                           ]
          vV = va_k - va_j
          vAxis = unit (va_j ^-^ va_i)  -- axis of rotation
          vrQ = rotateQ vAxis phi vV
       in (sNa, vrQ)
                                                           -}
                                                        --in (sNa, if (sNa /= "C5") then vr_n else va_i + vV)
       --in (sNa, HM.fromList [crr,1,1] )
    --fromZMat (sna:_) = (sna,HM.fromList [9,0,0])
    --vUnity :: V3 Double -> V3 Double
    --vUnity v = HM.scale (1/(HM.norm_2 v)) v
    vUnity v = v ^/ (norm v)
    callVar :: String -> Double
    callVar s = fromJust $ M.lookup s vMap
    callVarR s = callVar s
    callVarDih s = deg2rad $ callVar s
    --callVarAngle s = deg2rad $ abs $ (flip remD) 180 $ callVar s
    callVarAngle s = abs $ (flip remD) 180 $ callVar s

rotateQ axis thetaDeg v = rotate q v
  where
    q = axisAngle axis $ deg2rad thetaDeg

remD a b = (fromIntegral $ rem (floor a) (floor b))
-- remD a b = a - (fromIntegral $ rem (floor a) (floor b))

type Angle = Double
type Length = Double
type Dihedral = Double



data Opts = Opts {
    _outFormat    :: String,
    _outDir       :: FilePath,
    _expression   :: String,
    _absolutePath :: Bool
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
             <*> switch    (long "absolute" <> short 'a' <>
                            help "output the name of Diagram in Haskell snippet as absolute path")

withHelp :: ParserInfo Opts
withHelp = info
       (helper <*> optsParser)
       (fullDesc <> progDesc "interpret inline Haskell code to insert images in Pandoc output\nhttps://github.com/bergey/diagrams-pandoc"
       <> header "diagrams-pandoc - a Pandoc filter for inline Diagrams")
