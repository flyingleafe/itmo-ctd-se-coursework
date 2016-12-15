{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

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
    getCollection :: KnownNat d => a -> IO (Maybe (DocCollection d))

newtype TextDirectoryParser = TDParser FilePath

data AcidStateParser = ASParser

countTokens :: T.Text -> M.Map T.Text Integer
countTokens = foldl' (\m w -> M.insertWith (+) w 1 m) M.empty . T.words

toSized :: KnownNat n => [a] -> VS.Vector n a
toSized = fromJust . VS.fromList

mergeCounts :: forall d. KnownNat d => [M.Map T.Text Integer] -> DocCollection d
mergeCounts ms = toSized lists
    where lists = map listT ms
          listT m = SBOW $ M.toList $ M.mapKeys (dict M.!) m
          dict = M.fromAscList $ (`zip` [1..]) $ M.keys $ M.unionsWith (+) ms

instance Preprocessing TextDirectoryParser where
    getCollection (TDParser fp) = do
        isOk <- isDirectory fp
        if isOk
        then Just <$> do
            docFilePaths <- filter (`hasExtension` "txt") <$> listDirectory fp
            counts <- mapM (fmap countTokens <$> readTextFile) docFilePaths
            return $ mergeCounts counts
        else return Nothing
