{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

module Pipeline
       ( Pipe (..)
       , MonadPipeline (..)
       , Source
       , Sink
       , Closed
       , runSource
       ) where

import           Control.Arrow
import           Control.Category (Category (..))
import           Universum        hiding ((.))

newtype Pipe r e a b = Pipe
    { runPipe :: (a, r) -> Either e b
    }

instance Category (Pipe r e) where
    Pipe f . Pipe g = Pipe $
        \(a, r) -> g (a, r) >>= f . (, r)
    id = Pipe $ pure . fst

instance Arrow (Pipe r e) where
    arr f = Pipe $ pure . f . fst
    first (Pipe f) = Pipe $ \((b, d), r) -> (, d) <$> f (b, r)

class Monad m => MonadPipeline r e m where
    type Input m :: *
    type Output m :: *

    pipe :: Pipe r e (Input m) (Output m)

type Source r e b = Pipe r e () b
type Sink r e a = Pipe r e a ()
type Closed r e = Pipe r e () ()

runSource :: Source r e b -> r -> Either e b
runSource s r = runPipe s ((), r)
