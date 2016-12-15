module Main where

import           Data.String   (fromString)
import           Universum

import           Control.Arrow
import           DocSource
import           Model
import           OutputSink
import           Pipeline

main :: IO ()
main = do
    (path:_) <- getArgs
    let conf = TMConfig $ fromString path
        pipeline = pipeDocSource @TextDirectorySource >>>
                   pipeModel @MockModel >>>
                   pipeOutputSink @MockSink
    res <- runSource pipeline conf
    case res of
        Left err -> putText $ "Error: " <> err
        Right _  -> putText "Finished!"
