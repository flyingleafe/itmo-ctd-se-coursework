{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Constraints
       ( PipelineMode
       , RunnableMode
       , TMError
       , ErrorTM
       ) where

import           Control.Monad.Except (ExceptT)
import           Universum

import           Config               (TMConfig)
import           Pipeline

type TMError = Text

type RunnableMode m = MonadRunnable TMConfig TMError IO m

type PipelineMode inp outp m
    = ( RunnableMode m
      , MonadPipeline TMConfig TMError IO inp outp m
      )

type ErrorTM = ExceptT TMError IO
