{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Preprocessing where

import           Prelude hiding (FilePath)
import           Data.Type.Natural
import           GHC.TypeLits
import qualified Data.Vector.Sized            as VS
import           Numeric.LinearAlgebra.Static
import           Data.Maybe (fromJust)
import qualified Data.Text.IO as TIO
import           Model
import           Filesystem.Path
import           Filesystem
import qualified Data.Text as T
import qualified Data.Map as M
import           Control.Monad (mapM)

class Preprocessing a where
    getCollection :: (KnownNat w, KnownNat d) => a -> IO (Maybe (DocCollection w d))

newtype TextDirectoryParser = TDParser FilePath

newtype AcidStateParser = ASParser String -- String is a hole here

countTokens :: T.Text -> M.Map T.Text Integer
countTokens = undefined . T.words

mergeCounts :: (KnownNat w, KnownNat d) => [M.Map T.Text Integer] -> DocCollection w d
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
