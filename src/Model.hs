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
       ( WordId
       , ModelScore(..)
       , SparseBOW(..)
       , DocCollection(..)
       , ModelParams(..)
       , ModelOutput(..)
       , MockModel(..)
       , mkStochastic
       ) where

import Control.Arrow (arr)
import           Data.Default
import qualified Data.Vector.Sized            as VS
import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static
import Universum hiding ((<>))

import Pipeline
import Config
import Constraints

type WordId = Int

-- TODO: Move 10 into "integer type constants"
data ModelScore t = PerplexityScore [Double]
                  | forall s. (KnownNat t, KnownNat s, s <= 10) =>
                        TopWordsScore (VS.Vector t (VS.Vector s WordId))

data SparseBOW t = SBOW { unSBOW :: [(t, Integer)] }

type DocCollection d = VS.Vector d (SparseBOW WordId)

data ModelParams w t d = (KnownNat w, KnownNat t, KnownNat d) =>
    MP { documents    :: DocCollection d
       , initialPhi   :: L w t
       , initialTheta :: L t d
       }

data ModelOutput w t d = (KnownNat w, KnownNat t, KnownNat d) =>
    MO { outputPhi   :: L w t
       , outputTheta :: L t d
       , scores      :: [ModelScore t]
       }

newtype MockModel a = MockModel
    { getMockModel :: IO a
    } deriving (Functor, Applicative, Monad, MonadIO)

instance MonadRunnable TMConfig TMError IO MockModel where
    runE _ = fmap Right . getMockModel

instance MonadPipeline TMConfig TMError IO (ModelParams w t d) (ModelOutput w t d) MockModel where
    pipe = arr buildModel
      where buildModel MP{..} =
                MO { outputPhi = initialPhi
                   , outputTheta = initialTheta
                   , scores = []
                   }

instance (KnownNat w, KnownNat t, KnownNat d) =>
    Default (ModelParams w t d) where
    def = MP { documents = undefined
             , initialPhi = mkStochastic 1
             , initialTheta = mkStochastic 2
             }

mkStochastic :: forall m n. (KnownNat m, KnownNat n) => Seed -> L m n
mkStochastic seed = m <> norms
    where norms = diag $ vector $ map ((1/) . norm_1) $ toColumns m
          m = uniformSample seed 0 1
