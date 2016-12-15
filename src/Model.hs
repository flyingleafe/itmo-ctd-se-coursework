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

import           Control.Arrow                (arr)
import           Data.Default
import qualified Data.Vector.Sized            as VS
import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static
import           Prelude                      (show)
import           Universum                    hiding (show, (<>))

import           Config
import           Constraints
import           Pipeline

type WordId = Int

data ModelScore t = PerplexityScore [Double]
                  | TopWordsScore (VS.Vector t [WordId])
                  deriving (Show)

data SparseBOW t = SBOW
    { unSBOW :: [(t, Integer)]
    } deriving (Show)

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

instance Show (ModelOutput w t d) where
    show MO {..} = "phi: " ++ show outputPhi ++ ", theta: " ++
                   show outputTheta ++ ", scores: " ++ show scores

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
