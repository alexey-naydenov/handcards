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
  returnA -< Hcd.Arguments { Hcd._fileName = fileName }
        

main :: IO ()
main = do
    arguments <- execParser fullArgs
    Hcc.runCmd arguments
  where
    fullArgs = info (args <**> helper)
      ( fullDesc
     <> progDesc "Split hand drawn flashcards"
     <> header "handcards - a program to split hand drawn flashcards" )
