module Main where

import           Data.Default (def)
import           Data.String  (fromString)
import           Universum

import           DocSource
import           Model
import           OutputSink
import           Types

main :: IO ()
main = do
    (path:_) <- getArgs
    let fp = fromString path
    res <- runBase def $ runTDSource fp -- >>= runMockModel >>= runMockSink
    case res of
        Left err -> putText $ "Error: " <> err
        Right _  -> putText "Finished!"
