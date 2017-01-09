{-# LANGUAGE TemplateHaskell #-}

module Config
       ( TMConfig (..)
       ) where

import           Data.Aeson    (FromJSON (..), ToJSON (..))
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Prelude       (show)
import           Universum     hiding (show)

-- | TODO: napihat' huyni
data TMConfig = TMConfig
    { tmDocFilePath :: FilePath
    }

deriveJSON defaultOptions ''TMConfig
