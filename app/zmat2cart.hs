#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/hascpvo/stack.yaml

{-# LANGUAGE FlexibleContexts #-}

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
import qualified Numeric.LinearAlgebra as HM (norm_2,scale)
import qualified Numeric.LinearAlgebra.Data as HM (fromList, Vector)

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
    putStrLn $ unlines $ map show $ toList $ genCart varMap vInit0 dInit struct $ fromList []
    -- putStrLn $ show $ genCart [0,0,0] [0,0,0] struct $ fromList []
--    putStrLn $ drawTree $ fmap show (Node 1 [Node 2 [], Node 3 []])

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
      let [rN,aN,dN] = map callVar [rNs,aNs,dNs]
          [crr,cra,crd] = map (snd . (index res) . floor . callVar) [rNref,aNref,dNref]
       in (sNa, HM.fromList [rN, aN, dN] )
    --fromZMat (sna:_) = (sna,HM.fromList [9,0,0])
    vUnity :: HM.Vector Double -> HM.Vector Double
    vUnity v = HM.scale (1/(HM.norm_2 v)) v
    callVar :: String -> Double
    callVar s = fromJust $ M.lookup s vMap
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
