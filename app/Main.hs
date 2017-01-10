module Main where

import           Data.Default        (def)
import           Data.String         (fromString)
import           Options.Applicative (Parser, ParserInfo, auto, command, execParser,
                                      fullDesc, help, helper, info, long, metavar, option,
                                      progDesc, short, strOption, subparser, value)
import           Universum

import           DocSource
import           Model
import           OutputSink
import           Runner
import           Types
import           Web.Server          (webServer)

data AppMode = CLIMode { datasetDir :: FilePath }
             | ServerMode { listenPort :: Word16 }

appModeParser :: Parser AppMode
appModeParser = subparser $
    command "cli" (info cliOpts $ progDesc "Run once in CLI mode") <>
    command "serve" (info serveOpts $ progDesc "Serve HTTP API to interact with client")

cliOpts :: Parser AppMode
cliOpts = CLIMode <$>
    strOption (long "dataset-dir" <>
               short 'd' <>
               metavar "FILEPATH" <>
               help "Path to dataset")

serveOpts :: Parser AppMode
serveOpts = ServerMode <$>
    option auto (long "port" <>
                 short 'p' <>
                 metavar "PORT" <>
                 value 8090 <>
                 help "Port to serve")

appModeInfo :: ParserInfo AppMode
appModeInfo = info (helper <*> appModeParser) $
    fullDesc `mappend` progDesc "Server part of document-clustering application"

main :: IO ()
main = do
    mode <- execParser appModeInfo
    res <- runBase def $ case mode of
        CLIMode path    -> putText "Running CLI mode" >> fitPipeline path
        ServerMode port -> webServer port
    case res of
        Right _  -> putText "Finished!"
        Left err -> putText $ "Error: " <> err
