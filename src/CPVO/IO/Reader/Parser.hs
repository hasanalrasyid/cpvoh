{-# LANGUAGE OverloadedStrings #-}

module CPVO.IO.Reader.Parser
where

import CPVO.Data
import Data.Attoparsec.Text
import Numeric.LinearAlgebra.Data hiding (double)
import Data.Text hiding (map,count) -- (pack)
import Data.Either
import Prelude hiding (unwords)

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
  return $ Atom a1 a2 a3 a4 a5 a6 a7

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

parseCellParam :: Parser (Int, Celldm)
parseCellParam = do
  a1 <- decimal
  skipSpace
  c1 <- double
  skipSpace
  c2 <- double
  skipSpace
  c3 <- double
  skipSpace
  c4 <- double
  skipSpace
  c5 <- double
  skipSpace
  c6 <- double
  skipWhile (not . isEndOfLine)
  return $ (a1, (c1,c2,c3,c4,c5,c6))

parseOneRow :: (Parser a) -> [[[Text]]] -> [[a]]
parseOneRow parser i = map (\tt -> rights $ map (\yy -> parseOnly parser $ unwords yy) tt) i

