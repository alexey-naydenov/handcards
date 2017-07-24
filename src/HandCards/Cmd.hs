module HandCards.Cmd where

import qualified HandCards.Data as Hcd

import qualified Codec.Picture as Cp
import qualified Codec.Picture.Repa as Cpr
import qualified Data.Array.Repa as R

runCmd :: Hcd.Arguments -> IO ()
runCmd args = do
  eimg <- Cpr.readImageRGB $ Hcd._fileName args
  case eimg of
    Left err -> putStrLn $ "Could not read image: " ++ err
    Right img -> do
      putStrLn $ "Read image: " ++ Hcd._fileName args
      (putStrLn . show . R.extent . Cpr.imgData) img

splitCards :: Cp.DynamicImage -> [Cp.Image Cp.PixelRGBA8]
splitCards dynamicImage =
  let rgbImage = Cp.convertRGBA8 dynamicImage;
      width = Cp.imageWidth rgbImage;
      height = Cp.imageHeight rgbImage;
      pixels = Cp.imageData rgbImage in
        []

