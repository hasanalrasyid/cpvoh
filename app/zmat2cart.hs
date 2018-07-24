#!/usr/bin/env stack
--stack --resolver lts-11.3 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/hascpvo/stack.yaml

module Main where

import           Options.Applicative
import           Text.Pandoc.JSON
import Data.Semigroup

import System.IO (getContents)
import Data.List.Split (splitOn)
import Language.Fortran.Parser.Utils (readReal)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M (fromList,lookup)
import Data.Tree (Tree(Node),drawTree)

main :: IO ()
main = do
    opts <- execParser withHelp
    z <- getContents
    putStrLn $ show opts
    let [struct,vars] = map (filter (not . null)) $ splitOn [["Variables:"]] $ drop 5 $ map words $ lines $ z
        varMap = M.fromList $ map (\[a,b] -> (a, fromJust $ readReal b)) vars
    putStrLn $ show $ M.lookup "R6" varMap
    putStrLn $ show $ genCart [0,0,0] struct []
--    putStrLn $ drawTree $ fmap show (Node 1 [Node 2 [], Node 3 []])

genCart v0 [] res = res
genCart v0 ([s1]:xs) _ = genCart v0 xs [(s1, v0)]
genCart v0 ([s2a,s2b,s2c]:xs) res = genCart v0 xs ((s2a,[1,0,0]):res)
genCart v0 ([s3a,s3b,s3c,s3d,s3e]:xs) res = genCart v0 xs ((s3a,[2,0,0]):res)
genCart v0 ((sna:_):xs) res = genCart v0 xs ((sna,[9,0,0]):res)

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
