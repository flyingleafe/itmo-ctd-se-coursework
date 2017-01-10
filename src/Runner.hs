{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Runner
       ( MonadFork (..)
       , fork_
       , fitPipeline
       , predictPipeline
       ) where

import           Control.Concurrent   (ThreadId, forkIO)
import           Control.Monad.Except (ExceptT (..), runExceptT)
import           Control.Monad.Reader (MonadReader (..), ReaderT (..))
import           Control.Monad.Reader (ask)
import           Universum

import           DocSource
import           Model
import           OutputSink
import           Types

-----------------------------------------------------------------------
-- Forking logic
-----------------------------------------------------------------------

-- | Type family for thread IDs
type family TId (m :: * -> *) :: *

-- | Monads which can "fork" (whatever it means)
class Monad m => MonadFork m where
    fork :: m () -> m (TId m)

fork_ :: MonadFork m => m () -> m ()
fork_ action = () <$ fork action

-- | Straightforward IO instance
instance MonadFork IO where
    fork = forkIO

type instance TId IO = ThreadId

-- | Exception to inject `Either` errors from `ExceptT` in lower `MonadCatch`
data ExceptTException e = ExceptTException e deriving (Show, Typeable)

instance (Typeable e, Show e) => Exception (ExceptTException e)

-- | Function to fold `ExceptT` into lower `MonadCatch`
toException :: (Typeable e, Show e, MonadCatch m) => ExceptT e m a -> m a
toException action = runExceptT action >>= either throwExc pure
  where throwExc = throwM . ExceptTException

-- | Function to raise `ExceptTException` to `ExceptT`
fromException :: (Typeable e, Show e, MonadCatch m) => m a -> ExceptT e m a
fromException action = ExceptT $
    (Right <$> action) `catch` \(ExceptTException e) -> return $ Left e

-- | `ExceptT` instance
instance (Typeable e, Show e, MonadFork m, MonadCatch m) =>
         MonadFork (ExceptT e m) where
    fork = fromException . fork . toException

type instance TId (ExceptT e m) = TId m

-- | `ReaderT` instance
instance MonadFork m => MonadFork (ReaderT r m) where
    fork m = ask >>= lift . fork . runReaderT m

type instance TId (ReaderT r m) = TId m

deriving instance MonadFork m => MonadFork (TVarStateT r m)
type instance TId (TVarStateT r m) = TId m

-------------------------------------------------------------------------------
-- Runners
-------------------------------------------------------------------------------

fitPipeline :: FilePath -> Base ()
fitPipeline = runTDSource >=> runKMeansModel >=> runMockSink

predictPipeline :: FilePath -> Base [[Double]]
predictPipeline = runTDSource >=> kMeansPredict . diCollection
