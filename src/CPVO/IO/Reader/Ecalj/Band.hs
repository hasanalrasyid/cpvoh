{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module CPVO.IO.Reader.Ecalj.Band where
import CPVO.Numeric
import CPVO.IO
import CPVO.IO.Type

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List.Split
import Data.List
import Data.Maybe
import Numeric.LinearAlgebra
import Data.Char (ord)
import Control.Monad.IO.Class (MonadIO)

getOrbital :: String -> OrbColumns
getOrbital x = case x of
                  "s"      -> Right [2]
                  "p"      -> Right [3,4,5]
                  "py"     -> Right [3]
                  "pz"     -> Right [4]
                  "px"     -> Right [5]
                  "d"      -> Right [6,7,8,9,10]
                  "eg"     -> Right [8,10]
                  "t2g"    -> Right [6,7,9]
                  "dxy"    -> Right [6]
                  "dyz"    -> Right [7]
                  "dz2"    -> Right [8]
                  "dxz"    -> Right [9]
                  "dx2My2" -> Right [10]
                  _        -> Left $ ErrCPVO $ "defOrbital Unknown" ++ x

