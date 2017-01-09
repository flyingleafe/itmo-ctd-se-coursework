module OutputSink
       ( runMockSink
       ) where

import           Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import           GHC.TypeLits         (KnownNat)
import           Universum

import           Config
import           Model                (ModelOutput (..))
import           Types

newtype MockSink a = MockSink
    { getMockSink :: Base a
    } deriving (Functor, Applicative, Monad, MonadIO,
                MonadError TMError)

runMockSink :: ModelOutput -> Base ()
runMockSink = getMockSink . liftIO . print
