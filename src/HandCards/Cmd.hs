module HandCards.Cmd where

import HandCards.Utils
import qualified HandCards.Data as Hcd

import qualified Codec.Picture as Cp
import qualified System.Directory as D
import qualified System.FilePath as P

runCmd :: Hcd.Arguments -> IO ()
runCmd args@Hcd.SplitArgs {} = do
  eimg <- Cp.readImage $ Hcd.inputImgFile args
  case eimg of
    Left err -> putStrLn $ "Fail to read image: " ++ err
    Right dimg -> do
      putStrLn $ "Splitting image: " ++ Hcd.inputImgFile args
      hashString <- calculateHash (Hcd.inputImgFile args)
      -- splitImage (P.joinPath [(Hcd.outputCardDir args), hashString])
      --            verticalLines horizontalLines dimg
      print horizontal
      print  [hasHorizontalLine array2d h | h <- horizontal]
      -- print horizontalLines
        where
          array2d = fromImage dimg
          (byWidth, byHeight) = collapseDimensions array2d
          vertical = findPeaks (Hcd.baseQuantile args)
                     (Hcd.peakQuantile args)
                     byHeight
          horizontal = findPeaks (Hcd.baseQuantile args)
                       (Hcd.peakQuantile args)
                       byWidth
          -- verticalLines = [v | v <- vertical, hasVerticalLine array2d v ]
          -- horizontalLines = [h | h <- horizontal, hasHorizontalLine array2d h]

runCmd args@Hcd.MakeArgs {} = do
  allFiles <- D.listDirectory $ Hcd.inputCardDir args
  writeFile (Hcd.outputAnkiFile args) ""
  mapM_ (appendFile $ Hcd.outputAnkiFile args)
        (fmap formAnkiLine $ getCardPairs allFiles)
