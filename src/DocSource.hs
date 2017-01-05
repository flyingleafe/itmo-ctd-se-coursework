module DocSource
       ( runTDSource
       ) where

import           Control.Monad                (mapM, unless)
import           Control.Monad.Except         (ExceptT, MonadError, runExceptT,
                                               throwError)
import qualified Data.Map                     as M
import           Data.Maybe                   (fromJust)
import           Data.Proxy                   (Proxy (..))
import qualified Data.Text                    as T
import           Data.Type.Natural
import           Filesystem
import           Filesystem.Path              (FilePath, hasExtension)
import           Formatting                   (sformat, shown, stext, (%))
import           GHC.TypeLits
import           Numeric.LinearAlgebra.Data
import           Prelude                      (show)
import           Universum                    hiding (FilePath, show)

import           Config                       (TMConfig (..))
import           Model
import           Types

newtype TextDirectorySource a = TDSource
    { getTDSource :: Base TMError a
    } deriving (Functor, Applicative, Monad, MonadIO,
                MonadError TMError)

countTokens :: T.Text -> M.Map T.Text Integer
countTokens = foldl' (\m w -> M.insertWith (+) w 1 m) M.empty . T.words

mergeCounts :: [M.Map T.Text Integer] -> DocCollection
mergeCounts ms = map listT ms
    where listT m = SBOW $ M.toList $ M.mapKeys (dict M.!) m
          dict = M.fromAscList $ (`zip` [1..]) $ M.keys $ M.unionsWith (+) ms

getTDCollection :: FilePath -> TextDirectorySource DocCollection
getTDCollection fp = do
    isOk <- liftIO $ isDirectory fp
    unless isOk $ throwError $
        sformat ("No directory on path "%shown) fp

    docFilePaths <- filter (`hasExtension` "txt") <$>
                    liftIO (listDirectory fp)
    counts <- mapM (fmap countTokens <$> liftIO . readTextFile) docFilePaths
    return $ mergeCounts counts

runTDSource :: FilePath -> Base TMError DocCollection
runTDSource = getTDSource . getTDCollection
