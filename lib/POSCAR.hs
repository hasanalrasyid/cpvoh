{-# LANGUAGE OverloadedStrings #-}

module POSCAR
where

import CPVO.Data.Type
import Data.Attoparsec.Text.Lazy
import Control.Applicative

import Numeric.LinearAlgebra.Data hiding (double)
--import Data.Text hiding (map,count) -- (pack)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Either

--skipLine :: Parser ()
--skipLine = void $ many (noneOf "\n") >> newline

parseAtom :: Parser Atom
parseAtom = do
  a1 <- decimal
  skipSpace
  a2 <- decimal
  skipSpace
  a3 <- double
  skipSpace
  a4 <- double
  skipSpace
  a5 <- double
  skipSpace
  a6 <- double
  skipSpace
  a7 <- double
--  skipWhile (not . isEndOfLine)
  return $ Atom a1 a2 a3 a4 a5 a6 a7 ""

parseSample :: Parser [Atoms]
parseSample = do
  a <- many1 parseSingleSample
  return a
    where
      parseSingleSample = do
        try skipSpace
        atom <- parseAtom
        skipWhile (not . isEndOfLine)
        coords <- count (totalNA atom) parseCart
        return $ Atoms atom coords

parseCart :: Parser Coord
parseCart = do
  try skipSpace
  a1 <- double
  skipSpace
  a2 <- double
  skipSpace
  a3 <- double
  rs <- takeTill isEndOfLine
  return $ Coord rs $ Cart (3 |> [a1,a2,a3])

parseCellParam :: Parser (Int, [Vars Double])
parseCellParam = do
  a1 <- decimal
  (c1:c2:c3:c4:c5:c6:_) <- count 6 constOrParam
  return $ (a1, [c1,c2,c3,c4,c5,c6])
  where
    constOrParam = takeParam <|> takeDouble
    takeParam = do
      skipSpace
      _ <- char '{'
      c <- double
      _ <- char '}'
      return $ ParamVar c
    takeDouble = do
      skipSpace
      c <- double
      return $ ConstVar c

parseOneRow :: (Parser a) -> [[[Text]]] -> [[a]]
parseOneRow parser i = map (\tt -> rights $ map (\yy -> parseOnly parser $ T.unwords yy) tt) i

