{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Model
       ( Model (..)
       , KMeansParams (..)
       , ModelOutput
       ) where

import qualified Data.Map  as M
import           Universum

import           Config
import           DocSource (TfIdfCollection)
import           Types

type ModelOutput = [Int]

-- | Typeclass for clustering models.
class Monad m => Model m p where
    prepareParams :: Int -> TfIdfCollection -> m p

    buildModel :: p -> m (p, ModelOutput)

    runModel :: Int -> TfIdfCollection -> m (p, ModelOutput)
    runModel nClusters = prepareParams nClusters >=> buildModel

type Point = [Double]

-- | Vector norm.
norm :: Point -> Double
norm = sqrt . foldl' acc 0
    where acc old new = old + (new ** 2)

-- | Dot product of two vectors.
dotProd :: Point -> Point -> Double
dotProd vec1 vec2 = foldl' (+) 0 $ zipWith (*) vec1 vec2

-- | Cosine metric function.
cosine :: Point -> Point -> Double
cosine vec1 vec2 = let
    dp = dotProd vec1 vec2
    mag = norm vec1 * norm vec2
 in dp / mag

-- | Euclidian metric function.
dist :: Point -> Point -> Double
dist a b = sqrt $ sum $ map (^2) $ zipWith (-) a b

-- | Assign points to closest centroids.
assign :: [Point] -> [Point] -> M.Map Point [Point]
assign centroids points = M.fromListWith (++)
                          [(assignPoint p, [p]) | p <- points]
  where assignPoint p = minimumBy (comparing (dist p)) centroids

-- | Relocate centroids to the middle of its group/
relocate :: M.Map Point [Point] -> M.Map Point [Point]
relocate = M.foldWithKey insertCenter M.empty
  where insertCenter _ ps = M.insert (center ps) ps
        center [] = [0,0]
        center ps = map average (transpose ps)
        average xs = sum xs / fromIntegral (length xs)

-- | Assign a cluster label to each point.
clusterize :: (Point -> Point -> Double) -> [Point] -> [Point] -> [Int]
clusterize r centroids = map assignPoint where
    assignPoint p = snd $ minimumBy (comparing (r p . fst)) (zip centroids [1..])

-- | K-Means model intermediate parameters.
data KMeansParams = KMeans { centroids :: [Point]
                           , points    :: TfIdfCollection
                           , metric    :: Point -> Point -> Double
                           }

-- | K-Means model implementation.
instance Model Base KMeansParams where
    prepareParams nClusters mx = return params where
        params = KMeans (take nClusters mx) mx cosine
    buildModel p@KMeans{..} =
      if converged
      then return (p, clusterize metric centroids points)
      else buildModel $ KMeans (M.keys newCentroidsMap) points metric
        where converged = all (< 0.00001) $
                zipWith dist (sort centroids) (M.keys newCentroidsMap)
              newCentroidsMap = relocate (assign centroids points)
