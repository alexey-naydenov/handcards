module HandCards.Data (Arguments (..)) where

data Arguments
  = SplitArgs { inputImgFile :: String,
                outputCardDir :: String,
                baseQuantile :: Double,
                peakQuantile :: Double }
  | MakeArgs { inputCardDir :: String,
               outputAnkiFile :: String }
  deriving (Eq, Show)
