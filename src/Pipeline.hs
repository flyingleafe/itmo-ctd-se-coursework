{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}

module Pipeline
       ( Pipe (..)
       , MonadRunnable (..)
       , MonadPipeline (..)
       , Source
       , Sink
       , Closed
       , runSource
       , pipe
       ) where

import           Control.Arrow
import           Control.Category     (Category (..))
import           Control.Monad.Except (ExceptT (..), runExceptT)
import           Data.Proxy           (Proxy)
import           Universum            hiding ((.))

newtype Pipe r e m a b = Pipe
    { runPipe :: (a, r) -> m (Either e b)
    }

instance Monad m => Category (Pipe r e m) where
    Pipe f . Pipe g = Pipe $
        \(a, r) -> runExceptT $ ExceptT (g (a, r)) >>= ExceptT . f . (, r)
    id = Pipe $ pure . pure . fst

instance Monad m => Arrow (Pipe r e m) where
    arr f = Pipe $ pure . pure . f . fst
    first (Pipe f) = Pipe $
        \((b, d), r) -> fmap (, d) <$> f (b, r)

class (Monad base, Monad m) => MonadRunnable r e base m where
    runE :: r -> m a -> base (Either e a)
    -- liftE :: base (Either e a) -> m a

class MonadRunnable r e base m =>
      MonadPipeline r e base inp outp m where
    pipeImpl :: inp -> r -> m outp

pipe :: forall r e base inp outp m. MonadPipeline r e base inp outp m
     => Pipe r e base inp outp
pipe = Pipe $ \(a, r) -> runE @r @e @base @m r $
    pipeImpl @r @e @base @inp @outp @m a r

type Source r e m b = Pipe r e m () b
type Sink r e m a = Pipe r e m a ()
type Closed r e m = Pipe r e m () ()

runSource :: Source r e m b -> r -> m (Either e b)
runSource s r = runPipe s ((), r)

-- | Useful common instances
instance Monad m => MonadRunnable r e m (ExceptT e m) where
    runE _ (ExceptT m) = m

-- instance MonadRunnable r e base m => MonadRunnable r e base (ExceptT e m) where
    -- runE r (ExceptT m) =
