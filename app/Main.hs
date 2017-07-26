{-# LANGUAGE Arrows #-}
module Main where

import qualified HandCards.Cmd as Hcc
import qualified HandCards.Data as Hcd

import Options.Applicative
import Options.Applicative.Arrows
import Data.Semigroup ((<>))

args :: Parser Hcd.Arguments
args = runA $ proc () -> do
  fileName <- asA (
    strOption ( long "input"
             <> short 'i'
             <> metavar "INPUT"
             <> help "input file name")) -< ()
  outputPrefix <- asA (
    strOption ( long "output"
             <> short 'o'
             <> metavar "OUTPUT"
             <> help "prefix for output files")) -< ()
  baseQuantile <- asA (
    option auto ( long "base"
               <> short 'b'
               <> value (0.3 :: Double)
               <> metavar "QUANTILE"
               <> help "quantile used to find line boundaries")) -< ()
  peakQuantile <- asA (
    option auto ( long "peak"
               <> short 'p'
               <> value (0.99 :: Double)
               <> metavar "QUANTILE"
               <> help "quantile used to identify lines")) -< ()
  returnA -< Hcd.Arguments { Hcd._fileName = fileName,
                             Hcd._outputPrefix = outputPrefix,
                             Hcd._baseQuantile = baseQuantile,
                             Hcd._peakQuantile = peakQuantile}
        

main :: IO ()
main = do
    arguments <- execParser fullArgs
    Hcc.runCmd arguments
  where
    fullArgs = info (args <**> helper)
      ( fullDesc
     <> progDesc "Split hand drawn flashcards"
     <> header "handcards - a program to split hand drawn flashcards" )
