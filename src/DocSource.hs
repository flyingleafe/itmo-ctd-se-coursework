{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module DocSource where

import           Control.Monad                (mapM, unless)
import           Control.Monad.Except         (ExceptT, MonadError, runExceptT,
                                               throwError)
import qualified Data.Map                     as M
import           Data.Maybe                   (fromJust)
import           Data.Proxy                   (Proxy (..))
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
import           Pipeline

class RunnableMode m => DocSource m where
    getCollection :: KnownNat d => TMConfig -> m (DocCollection d)

instance (KnownNat d, DocSource m) =>
         MonadPipeline TMConfig TMError IO () (DocCollection d) m where
    pipeImpl = const getCollection

type DocSourceMode m
    = ( SourceMode (forall d. KnownNat d => DocCollection d) IO
      , DocSource m
      )

pipeDocSource :: forall m d. DocSourceMode m d
              => Source TMConfig TMError IO (DocCollection d)
pipeDocSource = pipe @TMConfig @TMError @IO @() @(DocCollection d) @m

newtype TextDirectorySource a = TDSource
    { runTDParser :: ExceptT TMError IO a
    } deriving (Functor, Applicative, Monad, MonadIO,
                MonadError TMError, MonadRunnable r TMError IO)

countTokens :: T.Text -> M.Map T.Text Integer
countTokens = foldl' (\m w -> M.insertWith (+) w 1 m) M.empty . T.words

toSized :: KnownNat n => [a] -> VS.Vector n a
toSized = fromJust . VS.fromList

mergeCounts :: KnownNat d => [M.Map T.Text Integer] -> DocCollection d
mergeCounts ms = toSized lists
    where lists = map listT ms
          listT m = SBOW $ M.toList $ M.mapKeys (dict M.!) m
          dict = M.fromAscList $ (`zip` [1..]) $ M.keys $ M.unionsWith (+) ms

instance DocSource TextDirectorySource where
    getCollection TMConfig {..} = do
        isOk <- liftIO $ isDirectory tmDocFilePath
        unless isOk $ throwError $
            sformat ("No directory on path "%shown) tmDocFilePath

        docFilePaths <- filter (`hasExtension` "txt") <$>
                        liftIO (listDirectory tmDocFilePath)
        counts <- mapM (fmap countTokens <$> liftIO . readTextFile) docFilePaths
        return $ mergeCounts counts
