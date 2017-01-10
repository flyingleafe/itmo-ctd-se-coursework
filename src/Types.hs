{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Types
       ( TVarStateT (..)
       , Base
       , TMError
       , Point
       , DocCollection
       , TfIdfCollection
       , KMeansParams (..)

       , AppState (..)
       , ProcessData (..)
       , appState
       , metrics
       , labels

       , WorkMode
       , runBase
       , runBaseRaw
       ) where

import           Control.Concurrent.STM    (TVar, newTVar, readTVar, writeTVar)
import           Control.Lens              (makeLenses)
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
    get = TVarStateT ask >>= atomically . readTVar
    put s = TVarStateT ask >>= atomically . flip writeTVar s
    state f = TVarStateT ask >>= atomically . tx
      where tx tv = do
                s <- readTVar tv
                let (a, s') = f s
                writeTVar tv s'
                return a

type Point = [Double]
type DocCollection = [[Text]]
type TfIdfCollection = [[Double]]

-- | K-Means model intermediate parameters.
data KMeansParams = KMeans
    { centroids :: ![Point]
    , points    :: !TfIdfCollection
    } deriving (Eq, Show)

-- | Application state marker
data AppState = Await
              | Processing
              | Ready KMeansParams
              deriving (Eq, Show)

instance Default AppState where
    def = Await

-- | Application processing data
data ProcessData = ProcessData
    { _appState :: !AppState
    , _metrics  :: ![[Double]]
    , _labels   :: [Text]
    } deriving (Eq, Show)

makeLenses ''ProcessData

instance Default ProcessData where
    def = ProcessData def [] []

type TMError = Text
type Base = TVarStateT ProcessData (ExceptT TMError IO)

runBase :: ProcessData -> Base a -> IO (Either TMError a)
runBase pd action = do
    tv <- atomically $ newTVar pd
    runBaseRaw tv action

runBaseRaw :: TVar ProcessData -> Base a -> IO (Either TMError a)
runBaseRaw tv action = runExceptT $ flip runReaderT tv $ getTVarStateT action

-- | WorkMode constraint
type WorkMode m =
    ( MonadIO m
    , MonadError TMError m
    , MonadState ProcessData m
    )
