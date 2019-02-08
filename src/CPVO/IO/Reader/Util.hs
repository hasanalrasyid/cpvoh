{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}


module CPVO.IO.Reader.Util
  where

import CPVO.IO.Type

fromRydberg :: Double
fromRydberg=1/13.605


getInvStat a = if a < 0 then Flip
                        else Keep
invStat2Double Flip = (-1)
invStat2Double _    = 1
