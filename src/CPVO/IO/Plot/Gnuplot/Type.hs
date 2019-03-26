{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module CPVO.IO.Plot.Gnuplot.Type
  where

data PlotSetting = NullSetting | PlotSetting { _tailer :: String
                                 , _titles :: String
                                 , _yrange :: String
                                 , _xrange :: String
                                 , _ticks :: String
                                 , _arrow :: String
                                 , _xylabel :: String
                                 } deriving Show
defSetting = PlotSetting "" "" "" "" "" "" ""
