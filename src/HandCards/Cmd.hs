module HandCards.Cmd where

import qualified HandCards.Data as Hcd

runCmd :: Hcd.Arguments -> IO ()
runCmd args = putStrLn ("Processing file: " ++ Hcd._fileName args)
