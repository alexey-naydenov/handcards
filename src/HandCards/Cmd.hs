module HandCards.Cmd where

import qualified HandCards.Data as Hcd

import Data.Functor.Identity
import Data.Maybe
import qualified Codec.Picture as Cp
import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Algorithms.Intro as A

import qualified System.FilePath as P
import qualified Data.ByteString as B
import qualified Crypto.Hash as H

runCmd :: Hcd.Arguments -> IO ()
runCmd args = do
  eimg <- Cp.readImage $ Hcd._fileName args
  case eimg of
    Left err -> putStrLn $ "Could not read image: " ++ err
    Right dimg -> do
      putStrLn $ "Read image: " ++ Hcd._fileName args
      putStrLn "Found lines vertical/horizontal:"
      print vertical
      print horizontal
      hashString <- calculateHash (Hcd._fileName args)
      splitImage (P.joinPath [(Hcd._outputDir args), hashString])
                 vertical horizontal dimg
        where (byWidth, byHeight) = (collapseDimensions . fromImage) dimg
              vertical = findPeaks (Hcd._baseQuantile args)
                                   (Hcd._peakQuantile args)
                                   byHeight
              horizontal = findPeaks (Hcd._baseQuantile args)
                                     (Hcd._peakQuantile args)
                                     byWidth

calculateHash :: String -> IO String
calculateHash path = do
  bs <- B.readFile path
  return $ show (H.hash bs :: H.Digest H.SHA1)

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
                   else Just $ fromMaybe index begin
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
  reverse $ _peaks peaksAcc
  where
  (baseValue, peakValue) =
    fromMaybe (error "cannot calculate boundary values") $
      getPeakBounds baseQuantile peakQuantile vector
  peaksAcc = V.ifoldl' (accumulatePeaks baseValue peakValue)
                       (PeakAcc False Nothing [])
                       vector

splitImage :: String -> [(Int, Int)] -> [(Int, Int)] -> Cp.DynamicImage -> IO ()
splitImage prefix vertical horizontal dimg =
  do
    -- Cp.writePng (prefix ++ ".png") img
    mapM_ (uncurry Cp.writePng) images
  where img = Cp.convertRGBA8 dimg
        width = Cp.imageWidth img
        height = Cp.imageHeight img
        vBoundaries = cardBoundaries 0 (height - 1) horizontal
        hBoundaries = cardBoundaries 0 (width - 1) vertical
        makeName vi hi = prefix ++ "_" ++ (show vi) ++ "_" ++ (show hi) ++ ".png"
        images = [(makeName vi hi, extractImage img top bottom left right)
                 | (vi, top, bottom) <- vBoundaries,
                   (hi, left, right) <- hBoundaries]

extractImage :: (Cp.Pixel p) =>
  Cp.Image p -> Int -> Int -> Int -> Int -> Cp.Image p
extractImage img top bottom left right =
  Cp.generateImage (\ x y -> Cp.pixelAt img (left + x) (top + y))
                   (right - left) (bottom - top)

cardBoundaries :: Int -> Int -> [(Int, Int)] -> [(Int, Int, Int)]
cardBoundaries firstBoundary lastBoundary bands =
  zip3 [1..] (firstBoundary : begins) (ends ++ [lastBoundary])
  where
    (ends, begins) = unzip bands
