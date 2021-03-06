{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Model
       ( Model (..)
       , KMeansParams (..)
       , ModelOutput
       , runKMeansModel
       , kMeansPredict
       ) where

import           Control.Concurrent.STM.TVar (readTVar, writeTVar)
import           Control.Lens                (use, (%=))
import           Control.Monad.Except        (MonadError, throwError)
import           Control.Monad.State         (MonadState (..))
import qualified Data.Map                    as M
import           System.Random.Shuffle       (shuffleM)
import           Universum

import           Config
import           DocSource                   (DocumentsInfo (..))
import           Types

type ModelOutput = [Int]

-- | Typeclass for clustering models.
class Monad m => Model m p where
    prepareParams :: DocumentsInfo -> m p

    buildModel :: p -> m (p, ModelOutput)

    predictModel :: p -> TfIdfCollection -> m [[Double]]

    runModel :: DocumentsInfo -> m (p, ModelOutput)
    runModel = prepareParams >=> buildModel

-- | Vector norm.
norm :: Point -> Double
norm = sqrt . foldl' acc 0
  where acc old new = old + (new ** 2)

-- | Dot product of two vectors.
dotProd :: Point -> Point -> Double
dotProd vec1 vec2 = foldl' (+) 0 $ zipWith (*) vec1 vec2

-- | Cosine cosine function.
cosine :: Point -> Point -> Double
cosine vec1 vec2 =
    let dp = dotProd vec1 vec2
        mag = norm vec1 * norm vec2
    in 1 - dp / mag

-- | Euclidian cosine function.
dist :: Point -> Point -> Double
dist a b = sqrt $ sum $ map (^2) $ zipWith (-) a b

-- | Assign points to closest centroids.
assign :: (Point -> Point -> Double) -> [Point] -> [Point] -> M.Map Point [Point]
assign r centroids points = M.fromListWith (++)
                          [(assignPoint p, [p]) | p <- points]
  where assignPoint p = minimumBy (comparing (r p)) centroids

-- | Relocate centroids to the middle of its group/
relocate :: M.Map Point [Point] -> M.Map Point [Point]
relocate = M.foldWithKey insertCenter M.empty
  where insertCenter _ ps = M.insert (center ps) ps
        center [] = [0,0]
        center ps = map average (transpose ps)
        average xs = sum xs / fromIntegral (length xs)

-- | Assign a cluster label to each point.
clusterize :: (Point -> Point -> Double) -> [Point] -> [Point] -> [Int]
clusterize r centroids = map assignPoint
  where
    assignPoint p = snd $ minimumBy (comparing (r p . fst)) (zip centroids [1..])

-- | Inner cluster distance quality cosine.
innerClusterDist :: (Point -> Point -> Double) -> [Int] -> [Point] -> [Point] -> Double
innerClusterDist r ys centrs pts =
    sum $ map (\(y, c, p) -> r c p ** 2 / fromIntegral (cnt y)) (zip3 ys centrs pts)
  where cnt y = length $ filter (y ==) ys

-- | outer cluster (centroids) distance quality cosine.
outerCentersDist :: (Point -> Point -> Double) -> [Point] -> Double
outerCentersDist r centrs = coeff * sum (concatMap (\x -> map (r x) centrs) centrs)
  where n = length centrs
        coeff = 2 / fromIntegral n / fromIntegral (n - 1)

newtype KMeansModel a = KMeansModel
    { getKMeansModel :: Base a
    } deriving (Functor, Applicative, Monad, MonadIO,
                MonadState ProcessData, MonadError TMError)

-- | K-Means model implementation.
instance Model KMeansModel KMeansParams where
    prepareParams DocumentsInfo {..} = do
        -- TODO: check if they're not equal
        centroids <- take diNClusters <$> liftIO (shuffleM diCollection)
        return $ KMeans centroids diCollection
    buildModel p@KMeans{..} = do
        liftIO $ print $ maximum dists
        liftIO $ print clusters
        let !p1 = innerClusterDist cosine clusters centroids points
        let !p2 = outerCentersDist cosine centroids
        metrics %= ([p1, p2] :)
        if converged
            then return (p, clusters)
            else buildModel $ KMeans (M.keys newCentroidsMap) points
          where
            converged = maximum dists < 0.00001
            dists = zipWith cosine (sort centroids) (M.keys newCentroidsMap)
            newCentroidsMap = relocate (assign cosine centroids points)
            clusters = clusterize cosine centroids points
    predictModel KMeans{..} = return . map scorer
      where
        scorer point = map (cosine point) centroids

runKMeansModel :: DocumentsInfo -> Base (KMeansParams, ModelOutput)
runKMeansModel = getKMeansModel . runModel

kMeansPredict :: TfIdfCollection -> Base [[Double]]
kMeansPredict tc = getKMeansModel $ do
    st <- use appState
    case st of
        Ready ps -> predictModel ps tc
        _        -> throwError "Model is not ready yet"
