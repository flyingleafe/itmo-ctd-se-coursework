{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module OutputSink where

import           Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import           Universum

import           Config
import           Constraints
import           Model                (ModelOutput (..))
import           Pipeline

class RunnableMode m => OutputSink m where
    outputRes :: ModelOutput w t d -> TMConfig -> m ()

instance OutputSink m =>
         MonadPipeline TMConfig TMError IO (ModelOutput w t d) () m where
    pipe = pipeRunnable $ outputRes @m

newtype MockSink a = MockSink
    { runMockSink :: ExceptT TMError IO a
    } deriving (Functor, Applicative, Monad, MonadIO,
                MonadError TMError, MonadRunnable r TMError IO)

instance OutputSink MockSink where
    outputRes a _ = liftIO $ print a
