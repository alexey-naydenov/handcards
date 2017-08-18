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
      print horizontal
      print vertical
      print verticalLines
      splitImage (P.joinPath [(Hcd.outputCardDir args), hashString])
                 vertical horizontal dimg
        where
          array2d = fromImage dimg
          (byWidth, byHeight) = collapseDimensions array2d
          vertical = findPeaks (Hcd.baseQuantile args)
                     (Hcd.peakQuantile args)
                     byHeight
          horizontal = findPeaks (Hcd.baseQuantile args)
                       (Hcd.peakQuantile args)
                       byWidth
          -- horizontalLines = [h | h <- horizontal, hasHorizontalLine array2d h]
          verticalLines = [hasLine array2d v | v <- vertical]

runCmd args@Hcd.MakeArgs {} = do
  allFiles <- D.listDirectory $ Hcd.inputCardDir args
  writeFile (Hcd.outputAnkiFile args) ""
  mapM_ (appendFile $ Hcd.outputAnkiFile args)
        (fmap formAnkiLine $ getCardPairs allFiles)
