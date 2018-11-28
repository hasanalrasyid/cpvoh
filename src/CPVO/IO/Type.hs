{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}

module CPVO.IO.Type
  where

import Data.Either
--data ErrCPVO = ErrCPVO { err :: String }

data ErrCPVO = ErrCPVO {err :: String}
type OrbColumns = Either (ErrCPVO) ([Integer])

