{-# LANGUAGE RankNTypes #-}

module Model
       ( WordId
       , ModelScore(..)
       , SparseBOW(..)
       , DocCollection(..)
       , ModelParams(..)
       , ModelOutput(..)
       , MockModel(..)
       , mkStochastic
       , runMockModel
       ) where

import           Control.Arrow                (arr)
import           Control.Monad.Identity       (Identity, runIdentity)
import           Data.Default
import qualified Data.Vector.Sized            as VS
import           GHC.TypeLits
import           Numeric.LinearAlgebra.Static
import           Prelude                      (show)
import           Universum                    hiding (show, (<>))

import           Config
import           Types

-- | TODO: document all this types @tohnann
type WordId = Int

data ModelScore t = PerplexityScore [Double]
                  | TopWordsScore (VS.Vector t [WordId])
                  deriving (Show)

data SparseBOW t = SBOW
    { unSBOW :: [(t, Integer)]
    } deriving (Show)

type DocCollection d = VS.Vector d (SparseBOW WordId)

data ModelParams w t d = MP
    { documents    :: DocCollection d
    , initialPhi   :: L w t
    , initialTheta :: L t d
    } deriving (Show)

data ModelOutput w t d = MO
    { outputPhi   :: L w t
    , outputTheta :: L t d
    , scores      :: [ModelScore t]
    } deriving (Show)

mkStochastic :: forall m n. (KnownNat m, KnownNat n) => Seed -> L m n
mkStochastic seed = m <> norms
    where norms = diag $ vector $ map ((1/) . norm_1) $ toColumns m
          m = uniformSample seed 0 1

instance (KnownNat w, KnownNat t, KnownNat d) =>
    Default (ModelParams w t d) where
    def = MP { documents = undefined
             , initialPhi = mkStochastic 1
             , initialTheta = mkStochastic 2
             }

-- | Typeclass for models
class Monad m => Model m where
    prepareParams
        :: (KnownNat t, KnownNat w, KnownNat d)
        => DocCollection d -> m (ModelParams w t d)
    buildModel
        :: (KnownNat t, KnownNat w, KnownNat d)
        => ModelParams w t d -> m (ModelOutput w t d)
    runModel
        :: (KnownNat t, KnownNat w, KnownNat d)
        => DocCollection d -> m (ModelOutput w t d)
    runModel = prepareParams >=> buildModel

-- | Dummy model for tests (generates constant output)
newtype MockModel a = MockModel
    { getMockModel :: Identity a
    } deriving (Functor, Applicative, Monad)

instance Model MockModel where
    prepareParams = return . const def
    buildModel MP{..} = return $
        MO { outputPhi = initialPhi
           , outputTheta = initialTheta
           , scores = []
           }

runMockModel
    :: (KnownNat t, KnownNat w, KnownNat d)
    => DocCollection d -> Base TMError (ModelOutput w t d)
runMockModel = return . runIdentity . getMockModel . runModel
