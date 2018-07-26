#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/hascpvo/stack.yaml

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

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
import qualified Numeric.LinearAlgebra as HM (norm_2,scale,cross,toList)
import qualified Numeric.LinearAlgebra.Data as HM (fromList,fromRows,Vector,disps)

main :: IO ()
main = do
    opts <- execParser withHelp
    z <- getContents
    let vInit0 = HM.fromList [0,0,0] :: HM.Vector Double
    let dInit@(dInitDih,vInit1) =  (180, HM.fromList [1,0,0]) :: (Double,HM.Vector Double)
    putStrLn $ show opts
    let [struct,vars] = map (filter (not . null)) $ splitOn [["Variables:"]] $ drop 5 $ map words $ lines $ z
        varMap = M.fromList $ map (\[a,b] -> (a, fromJust $ readReal b)) vars
    putStrLn $ show $ M.lookup "R6" varMap
    putStrLn $ unlines $ map showCart $ toList $ genCart varMap vInit0 dInit struct $ fromList []
--    putStrLn $ show $ genCartG varMap vInit0 dInit struct
    -- putStrLn $ show $ genCart [0,0,0] [0,0,0] struct $ fromList []
--    putStrLn $ drawTree $ fmap show (Node 1 [Node 2 [], Node 3 []])

showVec :: HM.Vector Double -> String
showVec a = unwords $ map show $ HM.toList a

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


genCartG :: M.Map String Double -> HM.Vector Double -> (Double,HM.Vector Double) -> [[String]] -> ZCoord String
--genCartG _ _ _ [] res = res
--genCartG vMap v0 d1@(dih,v1) (x:xs) res = genCartG vMap v0 d1 xs $ insert res $ newNode x
genCartG vMap v0 d1@(dih,v1) (x:xs) =  NodeZ "n1" ref1r 1.1 ref1a 120.0 ref1d 120.0
  where ref1r = (Origin2 "or2"(Origin0 "or0") 1.5 (Origin1 "or1" 1.1) 120.0 )
        ref1a = (Origin1 "or1" 1.1)
        ref1d = (Origin0 "or0")

ZCoord = Tuple String (HM.Vector Double) deriving Show

showZCoord (ZCoord (nm,vec)) = nm ++ "===" ++ (show vec)
-}

genCart :: M.Map String Double -> HM.Vector Double -> (Double,HM.Vector Double) -> [[String]] -> Seq (String, HM.Vector Double) -> Seq (String, HM.Vector Double)
genCart _ _ _ [] res = res
genCart vMap v0 d1@(dih,v1) (x:xs) res = genCart vMap v0 d1 xs $ res |> fromZMat x
  where
    fromZMat :: [String] -> (String, HM.Vector Double)
    fromZMat [s1] = (s1,v0)
    fromZMat [s2a,_,r2] = (s2a, HM.scale ( callVar r2) $ vUnity v1)
    fromZMat [s3a,r3ref,r3,a3ref,a3] = let ca3 = callVar a3
                                           cr3 = callVar r3
                                           vCm1 = snd $ index res 1
                                        in (s3a,vCm1 + HM.scale cr3 ( HM.fromList [cos ca3,sin ca3 ,0] ))
    fromZMat (sNa:rNref:rNs:aNref:aNs:dNref:dNs:_) =
      let [rni,teta,phi] = map callVar [rNs,aNs,dNs]
          [va_i,va_j,va_k] = map (snd . (index res) . floor . (+ (-1)) . fromJust . readReal)
            [rNref,aNref,dNref] :: [HM.Vector Double]
          vb_ik = vUnity $ va_k - va_i
          vb_ij = vUnity $ va_j - va_i
          vb_ijk = HM.scale  (1/(sin teta)) $ HM.cross vb_ij vb_ik
          vr_n = (+) va_i
                $ HM.scale rni $ foldl (+) (HM.fromList [0,0,0])  [ HM.scale (cos teta) vb_ij
                                           , HM.scale ((sin teta) * (cos phi)) (HM.cross vb_ijk vb_ij)
                                           , HM.scale ((-1) * sin phi) vb_ijk
                                           ]
          in (sNa, vr_n)
       --in (sNa, HM.fromList [crr,1,1] )
    --fromZMat (sna:_) = (sna,HM.fromList [9,0,0])
    vUnity :: HM.Vector Double -> HM.Vector Double
    vUnity v = HM.scale (1/(HM.norm_2 v)) v
    callVar :: String -> Double
    callVar s = fromJust $ M.lookup s vMap

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
