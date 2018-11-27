module KMeans
    ( kMeans
    )
where

import Data.List (minimumBy, sortBy, groupBy)

class Ord a => Distance a where
    dist :: a -> a -> a

class Ord a => Mean a where
    mean :: [a] -> a

data KMeansError
    = InsufficientExample -- ^ K is greater than dataset size
    deriving (Eq, Show)

kMeans
    :: (Distance a, Mean a)
    => [a] -- ^ Dataset
    -> Int -- ^ K
    -> Either KMeansError [(a, a)] -- ^ Left KMeansError | Right [(centroid, example)]
kMeans xs k | length xs < k = Left InsufficientExample
            | otherwise     = Right $ kMeans' xs k

kMeans' :: (Distance a, Mean a) => [a] -> Int -> [(a, a)]
kMeans' xs k = go ys cs
  where
    ys = assignToCentroids xs cs
    cs = initialCentroids xs k

    go ys' cs' = if cs' == nCs then ys' else go (assignToCentroids xs nCs) nCs
        where nCs = newCentroids ys'

-- TODO: real initialization
initialCentroids :: [a] -> Int -> [a]
initialCentroids xs k = take k xs

assignToCentroids :: (Distance a, Mean a) => [a] -> [a] -> [(a, a)]
assignToCentroids xs cs = fmap (\x -> (closestCentroid x cs, x)) xs

closestCentroid :: Distance a => a -> [a] -> a
closestCentroid x = minimumBy (\a b -> compare (dist a x) (dist b x))

newCentroids :: Mean a => [(a, a)] -> [a]
newCentroids = fmap meanOfCluster . groupBy (\a b -> fst a == fst b) . sortBy
    (\a b -> compare (fst a) (fst b))

meanOfCluster :: Mean a => [(a, a)] -> a
meanOfCluster = mean . fmap snd
