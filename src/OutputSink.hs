module OutputSink
       ( runMockSink
       ) where

import           Control.Lens         ((.=))
import           Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import           Control.Monad.State  (MonadState)
import           GHC.TypeLits         (KnownNat)
import           Universum

import           Config
import           Model                (ModelOutput (..))
import           Types

newtype MockSink a = MockSink
    { getMockSink :: Base a
    } deriving (Functor, Applicative, Monad, MonadIO,
                MonadError TMError, MonadState ProcessData)

runMockSink :: (KMeansParams, ModelOutput) -> Base ()
runMockSink (p, mo) = getMockSink $ appState .= Ready p
