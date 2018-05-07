module CPVO.IO (
  showDouble
  ) where

import Text.Printf as TP

showDouble _ 0 = show 0
showDouble i x = (flip TP.printf) x $ concat ["%0.",show i,"f"]

