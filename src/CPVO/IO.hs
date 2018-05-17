{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module CPVO.IO (
    showDouble,
    markdownToTex,
    inshell2text,
    rendertable,
    takeAOs,
    flipBy,
    hashSpaceText
  ) where

import CPVO.Numeric

import Text.Printf as TP
import Text.Pandoc
import qualified Data.Text as T

import Turtle hiding (sortBy,char,text)
import qualified Control.Foldl as Fold
import Data.List

import Text.PrettyPrint.Boxes hiding ((<>),cols,rows)
import qualified Text.PrettyPrint.Boxes as TB
-- ===============================================
-- start of Accelerate
-- import Data.Array.Accelerate              as A
-- import Data.Array.Accelerate.LLVM.Native  as CPU
-- import Data.Array.Accelerate.LLVM.PTX     as GPU
--
{-
dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)

main1 = do
    kj@(texFile:jd:xr:ymax':wTot:tumpuk:invS:tailer:foldernya:aos) <- getArgs
    putStrLn $ show kj
--    let xs = fromList (Z:.10) [0..]   :: Vector Float
--    let ys = fromList (Z:.10) [1,3..] :: Vector Float
--    CPU.run $ dotp (use xs) (use ys)
-}

showDouble _ 0 = show 0
showDouble i x = (flip TP.printf) x $ concat ["%0.",show i,"f"]


-- Prosesor untuk pandoc
-- LaTeX writer...
--
markdownToTex str = runIOorExplode
              $ ((writeLaTeX def) <=< (readMarkdown def{
                  readerExtensions = foldr enableExtension pandocExtensions [Ext_tex_math_dollars, Ext_raw_tex, Ext_table_captions]
                                                       }))
              $ T.pack $ str
--------------------------------------------------------------------------------------
shell2list :: MonadIO io =>  Shell a->  io [a]
shell2list xx = fold (xx) Fold.list

shell2text :: MonadIO io => Shell Line -> io [Text]
shell2text xx = fmap (map lineToText) $ shell2list xx

inshell2text :: MonadIO io => String -> io [Text]
inshell2text xx = do
  (_,a,_) <- shellStrictWithErr ( T.pack xx) empty
  return $ T.lines a

--------------------------------------------------------------------------------------
-- Text Formatting
--
pad width x = x ++ replicate k ' '
  where k = width - length x

fmt_column :: [String] -> Box
fmt_column items = vcat left (addHead $ map (text.pad width) items)
  where width = maximum $ map length items
        hsep = text ( replicate width '-' )
        addHead (a:as) = a:hsep:as

--table :: [[String]] -> Box
rendertable rs = render $ vsep TB.<> hcat top (intersperse vsep (map fmt_column columns)) TB.<> vsep
  where
    columns = transpose rs
    nrows = length rs
    vsep =  vcat left $ map char ("|" ++ (concat $ replicate nrows "|"))

caller :: T.Text
caller = T.unlines  [ "callme with : genPDOSAtomicOrbital.hs [Atom:Orbital]                tailer    folder"
                    , "ex.         : genPDOSAtomicOrbital.hs 'O NiTd:2:3:4:5 CoTd:2:3:4:5' nico2o4   nico2o4.invB.0GGA"
                    ]

hashSpaceText t = T.unpack $ T.replace "#" " " $ T.pack t

---------------------------------------------------------------------------
getAllPDOS (s,n,pd) = do
  let pdTot = getY0 pd
  return (s,n,pdTot)

takeAOs _ [] = []
takeAOs k@(x,i) (ao@(n,l,m):as) = if (i == n) then [(x,(n,(l,m)))]
                                            else takeAOs k as

takeAO i [] = []
takeAO i (a@(n,_,_):as) = if (i == n) then [a]
                                     else takeAO i as

flipBy invStat x = if (invStat < 0) then reverse x
                                    else x

