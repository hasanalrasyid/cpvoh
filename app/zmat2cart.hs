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
import Text.Printf (printf)

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
showVec (V3 a b c) = unwords $ map (printf "%.6f") [a,b,c]


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
    vUnity v = v ^/ (norm v)
    callVar :: String -> Double
    callVar s = fromJust $ M.lookup s vMap
    callVarAngle s = abs $ (flip remD) 180 $ callVar s

remD a b = (fromIntegral $ rem (floor a) (floor b))


rotateQ axis thetaDeg v = rotate q v
  where
    q = axisAngle axis $ deg2rad thetaDeg

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
