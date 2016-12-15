module Preprocessing where

import           Control.Monad                (mapM)
import qualified Data.Map                     as M
import           Data.Maybe                   (fromJust)
import qualified Data.Text                    as T
import           Data.Type.Natural
import qualified Data.Vector.Sized            as VS
import           Filesystem
import           Filesystem.Path              (FilePath, hasExtension)
import           GHC.TypeLits
import           Model
import           Numeric.LinearAlgebra.Static
import           Universum                    hiding (FilePath)

class Preprocessing a where
    getCollection :: (KnownNat w, KnownNat d) => a -> IO (Maybe (DocCollection w d))

newtype TextDirectoryParser = TDParser FilePath

newtype AcidStateParser = ASParser Text -- String is a hole here

countTokens :: T.Text -> M.Map Text Integer
countTokens = undefined . T.words

mergeCounts :: (KnownNat w, KnownNat d) => [M.Map Text Integer] -> DocCollection w d
mergeCounts = undefined

instance Preprocessing TextDirectoryParser where
    getCollection (TDParser fp) = do
        isOk <- isDirectory fp
        if isOk
        then Just <$> do
            docFilePaths <- filter (`hasExtension` "txt") <$> listDirectory fp
            counts <- mapM (fmap countTokens <$> readTextFile) docFilePaths
            return $ mergeCounts counts
        else return Nothing
