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
import qualified Data.Vector.Sized            as VS
import           Filesystem
import           Filesystem.Path              (FilePath, hasExtension)
import           Formatting                   (sformat, shown, stext, (%))
import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static
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

toSized :: KnownNat n => [a] -> VS.Vector n a
toSized = fromJust . VS.fromList

mergeCounts :: KnownNat d => [M.Map T.Text Integer] -> DocCollection d
mergeCounts ms = toSized lists
    where lists = map listT ms
          listT m = SBOW $ M.toList $ M.mapKeys (dict M.!) m
          dict = M.fromAscList $ (`zip` [1..]) $ M.keys $ M.unionsWith (+) ms

getTDCollection :: KnownNat d => FilePath -> TextDirectorySource (DocCollection d)
getTDCollection fp = do
    isOk <- liftIO $ isDirectory fp
    unless isOk $ throwError $
        sformat ("No directory on path "%shown) fp

    docFilePaths <- filter (`hasExtension` "txt") <$>
                    liftIO (listDirectory fp)
    counts <- mapM (fmap countTokens <$> liftIO . readTextFile) docFilePaths
    return $ mergeCounts counts

runTDSource :: KnownNat d => FilePath -> Base TMError (DocCollection d)
runTDSource = getTDSource . getTDCollection
