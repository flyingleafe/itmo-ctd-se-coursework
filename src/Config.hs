module Config
       ( TMConfig (..)
       ) where

import           Filesystem.Path (FilePath)
import           Universum       hiding (FilePath)

-- | TODO: napihat' huyni
data TMConfig = TMConfig
    { tmDocFilePath :: FilePath
    }
