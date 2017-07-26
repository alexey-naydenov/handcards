module HandCards.Data where

data Arguments = Arguments
  { _fileName :: String,
    _outputDir :: String,
    _baseQuantile :: Double,
    _peakQuantile :: Double }
