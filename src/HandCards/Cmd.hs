module HandCards.Cmd where

import qualified HandCards.Data as Hcd

import Data.Word (Word8)
import Data.Functor.Identity
import qualified Control.Monad.ST as ST
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

collapseDimensions :: (R.Array R.D R.DIM2 Int)
                   -> ((R.Array R.U R.DIM1 Int), (R.Array R.U R.DIM1 Int))
collapseDimensions array =
  runIdentity $ do
  byWidth <- R.sumP array
  byHeight <- R.sumP $ R.transpose array
  return (byWidth, byHeight)

getPeakBounds :: (V.Unbox a, Ord a) =>
  Double -> Double -> (R.Array R.U R.DIM1 a) -> Maybe (a, a)
getPeakBounds baseQuantile peakQuantile array =
  case len of
    0 -> Nothing
    _ -> Just (sorted V.! baseIndex, sorted V.! peakIndex)
  where vector = R.toUnboxed array
        sorted = V.modify A.sort vector
        len = V.length vector
        baseIndex = floor $ baseQuantile * fromIntegral len :: Int
        peakIndex = floor $ peakQuantile * fromIntegral len :: Int
