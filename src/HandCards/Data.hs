module HandCards.Data where

data Arguments = Arguments
  { _fileName :: String,
    _outputPrefix :: String,
    _baseQuantile :: Double,
    _peakQuantile :: Double }
