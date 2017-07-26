module HandCards.Cmd where

import qualified HandCards.Data as Hcd

import Data.Functor.Identity
import Data.Maybe
import qualified Codec.Picture as Cp
import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Algorithms.Intro as A

runCmd :: Hcd.Arguments -> IO ()
runCmd args = do
  eimg <- Cp.readImage $ Hcd._fileName args
  case eimg of
    Left err -> putStrLn $ "Could not read image: " ++ err
    Right dimg -> do
      putStrLn $ "Read image: " ++ Hcd._fileName args
      -- (putStrLn . show . R.extent) byWidth
      -- (putStrLn . show) byWidth
      print $ getPeakBounds (Hcd._baseQuantile args) (Hcd._peakQuantile args)
                             byWidth
      print $ getPeakBounds (Hcd._baseQuantile args) (Hcd._peakQuantile args)
                             byHeight
      print $ findPeaks (Hcd._baseQuantile args) (Hcd._peakQuantile args)
                         byHeight
      print $ findPeaks (Hcd._baseQuantile args) (Hcd._peakQuantile args)
                         byWidth
        where (byWidth, byHeight) = (collapseDimensions . fromImage) dimg
                
fromImage :: Cp.DynamicImage -> R.Array R.D R.DIM2 Int
fromImage dimg =
  R.fromFunction (R.Z R.:. h R.:. w) (getPixel img)
  where img@Cp.Image { Cp.imageWidth = w, Cp.imageHeight = h, Cp.imageData = _}
          = Cp.convertRGBA8 dimg

getPixel :: (Cp.Image Cp.PixelRGBA8) -> R.DIM2 -> Int
getPixel img (R.Z R.:. y R.:. x) =
  fromIntegral r + fromIntegral g + fromIntegral b + fromIntegral a
  where (Cp.PixelRGBA8 r g b a) = Cp.pixelAt img x y

collapseDimensions :: (R.Array R.D R.DIM2 Int) -> (V.Vector Int, V.Vector Int)
collapseDimensions array =
  runIdentity $ do
  byWidth <- R.sumP array
  byHeight <- R.sumP $ R.transpose array
  return (R.toUnboxed byWidth, R.toUnboxed byHeight)

getPeakBounds :: (V.Unbox a, Ord a) =>
  Double -> Double -> V.Vector a -> Maybe (a, a)
getPeakBounds baseQuantile peakQuantile vector =
  case len of
    0 -> Nothing
    _ -> Just (sorted V.! baseIndex, sorted V.! peakIndex)
  where sorted = V.modify A.sort vector
        len = V.length vector
        baseIndex = floor $ baseQuantile * fromIntegral len :: Int
        peakIndex = floor $ peakQuantile * fromIntegral len :: Int

data PeakAcc = PeakAcc { _isInPeak::Bool
                       , _aboveBaseFirst::Maybe Int
                       , _peaks::[(Int, Int)] }

accumulatePeaks :: (Ord a) => a -> a -> PeakAcc -> Int -> a -> PeakAcc
accumulatePeaks baseValue peakValue
                PeakAcc {_isInPeak = False, _aboveBaseFirst = begin, _peaks = peaks}
                index value = PeakAcc {_isInPeak = value >= peakValue,
                                       _aboveBaseFirst = newBegin, _peaks = peaks}
  where newBegin = if value < baseValue
                   then Nothing
                   else case begin of
                     Nothing -> Just index
                     _ -> begin
accumulatePeaks baseValue _
                PeakAcc {_isInPeak = True, _aboveBaseFirst = Just begin,
                         _peaks = peaks}
                index value = PeakAcc {_isInPeak = value >= baseValue,
                                       _aboveBaseFirst = newBegin,
                                       _peaks = newPeaks}
  where newBegin = if value < baseValue
                   then Nothing
                   else Just begin
        newPeaks = if value < baseValue
                   then (begin, index):peaks
                   else peaks
accumulatePeaks _ _ _ _ _ = undefined


findPeaks :: (V.Unbox a, Ord a) =>
  Double -> Double -> (V.Vector a) -> [(Int, Int)]
findPeaks baseQuantile peakQuantile vector =
  _peaks peaksAcc
  where
  (baseValue, peakValue) =
    fromMaybe (error "cannot calculate boundary values") $
      getPeakBounds baseQuantile peakQuantile vector
  peaksAcc = V.ifoldl' (accumulatePeaks baseValue peakValue)
                       (PeakAcc False Nothing [])
                       vector

