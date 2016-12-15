{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Model
(
  WordId
, ModelScore(..)
, SparseBOW(..)
, ModelParams(..)
, ModelOutput(..)
, Model(..)
, MockModel(..)
, mkStochastic
) where

import           Data.Default
import qualified Data.Vector.Sized            as VS
import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static

type WordId = Int

-- TODO: Move 10 into "integer type constants"
data ModelScore t =
    PerplexityScore [Double]
  | forall s. (KnownNat t, KnownNat s, s <= 10) =>
      TopWordsScore (VS.Vector t (VS.Vector s WordId))

data SparseBOW w = forall s. (KnownNat w, KnownNat s, s <= w) =>
    SBOW { unSBOW :: VS.Vector s (WordId, Integer) }

data ModelParams w t d = (KnownNat w, KnownNat t, KnownNat d) =>
    MP { documents    :: VS.Vector d (SparseBOW w)
       , initialPhi   :: L w t
       , initialTheta :: L t d
       }

data ModelOutput w t d = (KnownNat w, KnownNat t, KnownNat d) =>
    MO { outputPhi   :: L w t
       , outputTheta :: L t d
       , scores      :: [ModelScore t]
       }

class Model a w t d where
    buildTopicModel :: a -> ModelParams w t d -> ModelOutput w t d

data MockModel = MockModel
data EMModel = EMModel

instance (KnownNat w, KnownNat t, KnownNat d) =>
    Default (ModelParams w t d) where
    def = MP { documents = undefined
             , initialPhi = mkStochastic 1
             , initialTheta = mkStochastic 2
             }

instance Model MockModel w t d where
    buildTopicModel _ MP{..} =
        MO { outputPhi = initialPhi
           , outputTheta = initialTheta
           , scores = []
           }

mkStochastic :: forall m n. (KnownNat m, KnownNat n) => Seed -> L m n
mkStochastic seed = m <> norms
    where norms = diag $ vector $ map ((1/) . norm_1) $ toColumns m
          m = uniformSample seed 0 1
