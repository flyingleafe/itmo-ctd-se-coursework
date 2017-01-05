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
import           Numeric.LinearAlgebra
import           Numeric.LinearAlgebra.Data
import           Prelude                      (show)
import           Universum                    hiding (show, (<>))

import           Config
import           Types

-- | TODO: document all this types @tohnann
type WordId = Int

-- | Score of ARTM model.
data ModelScore = PerplexityScore [Double]
                | TopWordsScore [WordId]
                  deriving (Show)

-- | Sparse Bag Of Words (BOW), used for efficiently
-- | storing rows of sparse integer matrices.
data SparseBOW t = SBOW
    { unSBOW :: [(t, Integer)]
    } deriving (Show)

-- | |W|*|D|-sized matrix containing amounts of words per document.
type DocCollection = [SparseBOW WordId]

-- | Initial parameters of ARTM model.
data ModelParams = MP
    { documents    :: DocCollection
    , initialPhi   :: Matrix R
    , initialTheta :: Matrix R
    } deriving (Show)

-- | Output of ARTM model.
data ModelOutput = MO
    { outputPhi   :: Matrix R
    , outputTheta :: Matrix R
    , scores      :: [ModelScore]
    } deriving (Show)

mkStochastic :: Seed -> Int -> Int -> Matrix R
mkStochastic seed rows cols = m <> norms
    where norms = diag $ vector $ map ((1/) . norm_1) $ toColumns m
          m = uniformSample seed rows (replicate cols (0, 1))

instance Default ModelParams where
    def = MP { documents = undefined
             , initialPhi = mkStochastic 1 100 20
             , initialTheta = mkStochastic 2 20 500
             }

-- | Typeclass for ARTM models.
class Monad m => Model m where
    prepareParams
        :: DocCollection -> m ModelParams
    buildModel
        :: ModelParams -> m ModelOutput
    runModel
        :: DocCollection -> m ModelOutput
    runModel = prepareParams >=> buildModel

-- | Dummy model for tests (generates constant output).
newtype MockModel a = MockModel
    { getMockModel :: Identity a
    } deriving (Functor, Applicative, Monad)

instance Model MockModel where
    prepareParams = return . const def
    buildModel MP{..} = return
        MO { outputPhi = initialPhi
           , outputTheta = initialTheta
           , scores = []
           }

runMockModel :: DocCollection -> Base TMError ModelOutput
runMockModel = return . runIdentity . getMockModel . runModel
