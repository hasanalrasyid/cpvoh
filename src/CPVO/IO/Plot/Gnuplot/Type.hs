{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module CPVO.IO.Plot.Gnuplot.Type
  where

data PlotSetting = NullSetting | PlotSetting { tailer :: String
                                 , titles :: String
                                 , yrange :: String
                                 , xrange :: String
                                 , ticks :: String
                                 , arrow :: String
                                 }
defSetting = PlotSetting "" "" "" "" "" ""
