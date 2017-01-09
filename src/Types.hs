{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types
       ( TVarStateT (..)
       , Base
       , TMError
       , AppState (..)
       , ProcessData (..)
       , runBase
       ) where

import           Control.Concurrent.STM    (TVar, newTVar, readTVar, writeTVar)
import           Control.Monad.Except      (ExceptT, MonadError, runExceptT)
import           Control.Monad.Reader      (ReaderT, ask, runReaderT)
import           Control.Monad.State       (MonadState (..))
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Data.Default              (Default (..))
import           Universum

-- | Abstract thread-safe state monad
newtype TVarStateT s m a = TVarStateT
    { getTVarStateT :: ReaderT (TVar s) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadError e,
                MonadIO, MonadThrow, MonadCatch, MonadMask, MonadFail)

instance MonadIO m => MonadState s (TVarStateT s m) where
    state f = TVarStateT ask >>= atomically . tx
      where tx tv = do
                s <- readTVar tv
                let (a, s') = f s
                writeTVar tv s'
                return a

-- | Application state marker
data AppState = Await
              | Processing
              | Ready

instance Default AppState where
    def = Await

-- | Application processing data
data ProcessData = ProcessData
    { appState :: AppState
    , dummy    :: Int
    }

instance Default ProcessData where
    def = ProcessData def 0

type TMError = Text
type Base = TVarStateT ProcessData (ExceptT TMError IO)

runBase :: ProcessData -> Base a -> IO (Either TMError a)
runBase pd action = do
    tv <- atomically $ newTVar pd
    runExceptT $ flip runReaderT tv $ getTVarStateT action
