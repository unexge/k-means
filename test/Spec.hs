{-# LANGUAGE FlexibleInstances #-}

import           Test.Hspec
import           KMeans

dataset :: [(Float, Float)]
dataset = [(1, 2), (2, 6), (1, 0)]

instance (Ord a, Fractional a) => Distance (a, a) where
    dist (a, b) (a', b') = (abs (a - a'), abs (b - b'))

instance (Ord a, Fractional a) => Mean (a, a) where
    mean xs = (f / l, s / l)
        where
            l = fromInteger . toInteger $ length xs
            (f, s) = foldr (\(x, y) (x', y') -> (x + x', y + y')) (0, 0) xs

main :: IO ()
main = hspec $ do
    describe "KMeans" $ do
        it "should cluster example dataset with K=2" $ do
            let (Right ys) = kMeans dataset 2
            (fmap fst ys) `shouldBe` [(1.0, 1.0), (2.0, 6.0), (1.0, 1.0)]
        it "should return InsufficientExample if K is greater than dataset" $ do
            kMeans dataset 5 `shouldBe` Left InsufficientExample
