module HandCards.Utils where

import Data.Functor.Identity
import Data.Maybe
import qualified Data.List as L
import qualified Codec.Picture as Cp
import qualified Data.Array.Repa as R
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Algorithms.Intro as A
import qualified Crypto.Hash as H
import qualified Data.ByteString as B
import Text.Regex

getCardPairs :: [String] -> [(String, String)]
getCardPairs paths =
  if (even . length) cards
  then splitInPairs cards
  else error "odd number of card files, something is wrong"
  where cards = L.sort [p | p <- paths, isCardFile p]

splitInPairs :: [a] -> [(a, a)]
splitInPairs [] = []
splitInPairs [x] = []
splitInPairs (x1:x2:xs) = (x1, x2) : splitInPairs xs

isCardFile path = isJust $ matchRegex ex path
  where ex = mkRegex(".*_[1,2]\\.(png)|(jpg)|(jpeg)$") :: Regex

formAnkiLine :: (String, String) -> String
formAnkiLine (front, back) =
  "<img src=\"" ++ front ++ "\">; <img src=\"" ++ back ++ "\">\n"

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

hasLine :: (R.Array R.D R.DIM2 Int) -> (Int, Int) -> Bool
hasLine array (minColumn, maxColumn) =
  runIdentity $ do
  return $ trueCount + (div trueCount 10) > V.length isSegmentVector
  where trueCount = countTrue isSegmentVector
        isSegmentVector = R.toUnboxed $ R.computeS isSegment
        isSegment = R.traverse centers removeTwo checkIfSegment
        centers = R.traverse array removeOuterDimension getCenter
        removeOuterDimension (sh R.:. _) = sh
        getCenter get row = Just $ sumProducts get row / sumValues get row
        sumValues get row =
          fromIntegral $ sum [ get (row R.:. c) | c <- [minColumn..maxColumn]]
        sumProducts get row =
          fromIntegral $ sum [ get (row R.:. c) * c | c <- [minColumn..maxColumn]]
        removeTwo (R.Z R.:. rowCount) = (R.Z R.:. (rowCount - 2))
        checkIfSegment get (R.Z R.:. row) =
          isLineSegment 5 (get (R.Z R.:. row)) (get (R.Z R.:. (row + 1))) (get (R.Z R.:. (row + 2)))

hasVerticalLine = hasLine

hasHorizontalLine :: (R.Array R.D R.DIM2 Int) -> (Int, Int) -> Bool
hasHorizontalLine arr range = hasLine (R.transpose arr) range

countTrue :: V.Vector Bool -> Int
countTrue v = V.foldl' (\ acc b -> if b then acc + 1 else acc) 0 v

isLineSegment :: Int -> Maybe Double -> Maybe Double -> Maybe Double -> Bool
isLineSegment maxJump (Just prev) (Just curr) (Just next) =
  abs (curr - prev) < fromIntegral maxJump
  && abs (curr - next) < fromIntegral maxJump
isLineSegment _ _ _ _ = False

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
