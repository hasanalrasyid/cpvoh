{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module CPVO.IO.Type
  where

--import Data.Either
--data ErrCPVO = ErrCPVO { err :: String }

data ErrCPVO = ErrCPVO {err :: String}
type OrbColumns = Either (ErrCPVO) ([Integer])

data InvStat = Keep | Flip deriving (Show)

type SpinID = Integer

type Directory = String

data Cetak = Cetak { spinID :: SpinID
                   , atom :: AtOrb
                   } deriving Show

-- uniqAtoms : [(count,noFirstAtom,atomicSymbol)]
data UniqueAtom = UA { count :: Int
                     , atnumUA :: AtNum
                     , atsymUA :: AtSym
                     } deriving (Show)

type AtNum = Int
type AtSym = String
data AtOrb = AO { atnum :: AtNum
                , atsym :: AtSym
                , labelAO :: String
                , intAOs :: [Int]
                } deriving (Show,Eq)

