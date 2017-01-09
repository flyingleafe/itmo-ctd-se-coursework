{-# LANGUAGE TemplateHaskell #-}

module Config
       ( TMConfig (..)
       ) where

import           Data.Aeson                (FromJSON (..), ToJSON (..))
import           Data.Aeson.TH             (defaultOptions, deriveJSON)
import           Filesystem.Path.CurrentOS (FilePath)
import           Prelude                   (show)
import           Universum                 hiding (FilePath, show)

-- | TODO: napihat' huyni
data TMConfig = TMConfig
    { tmDocFilePath :: FilePath
    }

instance FromJSON FilePath where
    parseJSON = fmap fromString . parseJSON

instance ToJSON FilePath where
    toJSON = toJSON . show

deriveJSON defaultOptions ''TMConfig
