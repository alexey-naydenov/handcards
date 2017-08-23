{-# LANGUAGE Arrows #-}
module Main where

import qualified HandCards.Cmd as Hcc
import qualified HandCards.Data as Hcd

import Options.Applicative
import Options.Applicative.Arrows
import Data.Semigroup ((<>))

splitArgs :: Parser Hcd.Arguments
splitArgs = runA $ proc () -> do
  fileName <- asA (
    strOption ( long "input"
             <> short 'i'
             <> metavar "INPUT"
             <> help "input file name")) -< ()
  outputDir <- asA (
    strOption ( long "output"
             <> short 'o'
             <> metavar "DIRECTORY"
             <> help "output directory")) -< ()
  baseQuantile <- asA (
    option auto ( long "base"
               <> short 'b'
               <> value (0.3 :: Double)
               <> metavar "QUANTILE"
               <> help "quantile used to find line boundaries")) -< ()
  peakQuantile <- asA (
    option auto ( long "peak"
               <> short 'p'
               <> value (0.95 :: Double)
               <> metavar "QUANTILE"
               <> help "quantile used to identify lines")) -< ()
  returnA -< Hcd.SplitArgs { Hcd.inputImgFile = fileName,
                             Hcd.outputCardDir = outputDir,
                             Hcd.baseQuantile = baseQuantile,
                             Hcd.peakQuantile = peakQuantile }

makeArgs :: Parser Hcd.Arguments
makeArgs = runA $ proc () -> do
  inputDir <- asA (
    strOption ( long "input"
             <> short 'i'
             <> metavar "DIRECTORY"
             <> help "directory that contains split cards")) -< ()
  outputFile <- asA (
    strOption ( long "output"
             <> short 'o'
             <> metavar "FILENAME"
             <> help "anki import file name")) -< ()
  returnA -< Hcd.MakeArgs { Hcd.inputCardDir = inputDir,
                            Hcd.outputAnkiFile = outputFile }

main :: IO ()
main = do
    arguments <- execParser opts
    Hcc.runCmd arguments
  where
    commandParser = subparser
      ( command "split" (info (splitArgs <**> helper)
                          (progDesc "Split hand drawn flashcards"))
     <> command "make" (info (makeArgs <**> helper)
                          (progDesc "Make anki file for all files in a dir"))
      )
    opts = info (commandParser <**> helper)
      (progDesc "Create anki flashcards from hand drawn notes")

