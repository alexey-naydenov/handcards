module HandCards.Cmd where

import qualified HandCards.Data as Hcd

import qualified Codec.Picture as Cp

runCmd :: Hcd.Arguments -> IO ()
runCmd args = do
  eimg <- Cp.readImage $ Hcd._fileName args
  case eimg of
    Left err -> putStrLn $ "Could not read image: " ++ err
    Right img -> do
      putStrLn $ "Read image: " ++ Hcd._fileName args
      (putStrLn . show . getImageInfo) img
        where cards = splitCards img


getImageInfo :: Cp.DynamicImage -> (Int, Int, String)
getImageInfo (Cp.ImageY8 img) =
  (Cp.imageWidth img, Cp.imageHeight img, "A greyscale image")
getImageInfo (Cp.ImageY16 img) =
  (Cp.imageWidth img, Cp.imageHeight img,
   "A greyscale image with 16bit components")
getImageInfo (Cp.ImageYF img) =
  (Cp.imageWidth img, Cp.imageHeight img,
   "A greyscale HDR image")
getImageInfo (Cp.ImageYA8 img) =
  (Cp.imageWidth img, Cp.imageHeight img,
   "An image in greyscale with an alpha channel")
getImageInfo (Cp.ImageYA16 img) =
  (Cp.imageWidth img, Cp.imageHeight img,
   "An image in greyscale with alpha channel on 16 bits")
getImageInfo (Cp.ImageRGB8 img) =
  (Cp.imageWidth img, Cp.imageHeight img,
   "An image in true color")
getImageInfo (Cp.ImageRGB16 img) =
  (Cp.imageWidth img, Cp.imageHeight img,
   "An image in true color with 16bit depth")
getImageInfo (Cp.ImageRGBF img) =
  (Cp.imageWidth img, Cp.imageHeight img,
   "An image with HDR pixels")
getImageInfo (Cp.ImageRGBA8 img) =
  (Cp.imageWidth img, Cp.imageHeight img,
   "An image in true color and an alpha channel")
getImageInfo (Cp.ImageRGBA16 img) =
  (Cp.imageWidth img, Cp.imageHeight img,
   "A true color image with alpha on 16 bits")
getImageInfo (Cp.ImageYCbCr8 img) =
  (Cp.imageWidth img, Cp.imageHeight img,
   "An image in the colorspace used by Jpeg images")
getImageInfo (Cp.ImageCMYK8 img) =
  (Cp.imageWidth img, Cp.imageHeight img,
   "An image in the colorspace CMYK")
getImageInfo (Cp.ImageCMYK16 img) =
  (Cp.imageWidth img, Cp.imageHeight img,
   "An image in the colorspace CMYK and 16 bits precision")

-- splitCards :: Cp.DynamicImage -> [Cp.Image Cp.PixelRGBA8]
-- splitCards dynamicImage =
--   let rgbImage = Cp.convertRGBA8 dynamicImage;
--       width = Cp.imageWidth rgbImage;
--       height = Cp.imageHeight rgbImage;
--       pixels = Cp.imageData rgbImage in
        []
