{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CPVO.IO (
    debugLine,
    showDouble,
    markdownToTex,
    inshell2text,
    system_,
    rendertable,
    takeAOs,
    flipBy,
    hashSpaceText
  ) where

import Text.Printf as TP
import Text.Pandoc
import qualified Data.Text as T

import Turtle hiding (sortBy,char,text)
import Data.List

import Text.PrettyPrint.Boxes hiding ((<>),cols,rows)
import qualified Text.PrettyPrint.Boxes as TB
import System.Process as SP

system_ :: String -> IO ()
system_ ss = SP.system ss >> return ()

showDouble :: Integer -> Double -> String
showDouble _ (0 :: Double) = show (0 :: Integer)
showDouble i a = (flip TP.printf) a $ concat ["%0.",show i,"f"]

debugLine :: Show a => Bool -> a -> IO ()
debugLine False _ = return ()
debugLine _ ss = putStrLn $ "===debug=== " ++ show ss


-- Prosesor untuk pandoc
-- LaTeX writer...
--
markdownToTex :: String -> IO Text
markdownToTex str = runIOorExplode
              $ ((writeLaTeX def) <=< (readMarkdown def{
                  readerExtensions = foldr enableExtension pandocExtensions [Ext_tex_math_dollars, Ext_raw_tex, Ext_table_captions]
                                                       }))
              $ T.pack $ str
--------------------------------------------------------------------------------------
inshell2text :: MonadIO io => String -> io [Text]
inshell2text xx = do
  (_,a,_) <- shellStrictWithErr ( T.pack xx) empty
  return $ T.lines a

--------------------------------------------------------------------------------------
-- Text Formatting
--
pad :: Int -> String -> String
pad width xx = xx ++ replicate k ' '
  where k = width - length xx

fmt_column :: [String] -> Box
fmt_column items = vcat left (addHead $ map (text.pad width) items)
  where width = maximum $ map length items
        hsepp = text ( replicate width '-' )
        addHead (a:as) = a:hsepp:as
        addHead _ = [hsepp]

--table :: [[String]] -> Box
rendertable :: [[String]] -> String
rendertable rs = render $ vsepp TB.<> hcat top (intersperse vsepp (map fmt_column columnss)) TB.<> vsepp
  where
    columnss = transpose rs
    nrows = length rs
    vsepp =  vcat left $ map char ("|" ++ (concat $ replicate nrows "|"))

hashSpaceText :: String -> String
hashSpaceText t = T.unpack $ T.replace "#" " " $ T.pack t

---------------------------------------------------------------------------

takeAOs :: (Int, String) -> [(String,String,[Int])] -> [(Int, (String, (String, [Int])))]
takeAOs _ [] = []
takeAOs k@(xx,i) ((n,ll,m):as) = if (i == n) then [(xx,(n,(ll,m)))]
                                           else takeAOs k as

flipBy :: Double -> [Int] -> [Int]
flipBy invStat xx = if (invStat < 0) then reverse xx
                                     else xx

