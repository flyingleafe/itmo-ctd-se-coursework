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
    { getMockSink :: Base TMError a
    } deriving (Functor, Applicative, Monad, MonadIO,
                MonadError TMError)

runMockSink
    :: (KnownNat t, KnownNat w, KnownNat d)
    => ModelOutput w t d -> Base TMError ()
runMockSink = getMockSink . liftIO . print
