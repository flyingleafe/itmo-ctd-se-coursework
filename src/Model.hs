{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Model
       (ModelOutput) where

import           Universum

import           Config
import           DocSource (TfIdfCollection)
import           Types

type DocCollection = [[Double]]
type ModelOutput = [Int]

-- | Typeclass for clustering models.
class Monad m => Model p m where
    prepareParams :: DocCollection -> m p

    buildModel :: p -> m (p, ModelOutput)

    runModel :: DocCollection -> m (p, ModelOutput)
    runModel = prepareParams >=> buildModel
