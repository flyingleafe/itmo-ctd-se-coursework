{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Preprocessing where

import           Control.Monad                (mapM, unless)
import           Control.Monad.Except         (ExceptT, MonadError, runExceptT,
                                               throwError)
import qualified Data.Map                     as M
import           Data.Maybe                   (fromJust)
import qualified Data.Text                    as T
import           Data.Type.Natural
import qualified Data.Vector.Sized            as VS
import           Filesystem
import           Filesystem.Path              (FilePath, hasExtension)
import           Formatting                   (sformat, shown, stext, (%))
import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static
import           Prelude                      (show)
import           Universum                    hiding (FilePath, show)

import           Config                       (TMConfig (..))
import           Constraints
import           Model
import           Pipeline                     (MonadPipeline (..), MonadRunnable (..),
                                               Pipe (..))

class RunnableMode m => Preprocessing m where
    getCollection :: (KnownNat w, KnownNat d) => TMConfig -> m (DocCollection w d)

instance (KnownNat w, KnownNat d, Preprocessing m) =>
         MonadPipeline TMConfig TMError IO () (DocCollection w d) m where
    pipe = Pipe $ \(_, conf) -> runE conf $ getCollection @m conf

newtype TextDirectoryParser a = TDParser
    { runTDParser :: ExceptT TMError IO a
    } deriving (Functor, Applicative, Monad, MonadIO,
                MonadError TMError, MonadRunnable r TMError IO)

newtype AcidStateParser = ASParser Text -- String is a hole here

countTokens :: T.Text -> M.Map Text Integer
countTokens = undefined . T.words

mergeCounts :: (KnownNat w, KnownNat d) => [M.Map Text Integer] -> DocCollection w d
mergeCounts = undefined

instance Preprocessing TextDirectoryParser where
    getCollection TMConfig {..} = do
        isOk <- liftIO $ isDirectory tmDocFilePath
        unless isOk $ throwError $
            sformat ("No directory on path "%shown) tmDocFilePath

        docFilePaths <- filter (`hasExtension` "txt") <$>
                        liftIO (listDirectory tmDocFilePath)
        counts <- mapM (fmap countTokens <$> liftIO . readTextFile) docFilePaths
        return $ mergeCounts counts
