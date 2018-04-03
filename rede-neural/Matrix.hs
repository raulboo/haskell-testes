module NeuralNetwork.Matrix
( matrixOperation
, dotProduct
, matrixMultiply
) where

import Data.List

matrixOperation :: Num a => (a -> a -> a) -> [[a]] -> [[a]] -> [[a]]
matrixOperation func m1 m2
  | length m1 /= length m2 = [[]]
  | otherwise = zipWith (zipWith func) m1 m2

dotProduct :: Num a => [a] -> [a] -> a
dotProduct v1 v2 = sum $ zipWith (*) v1 v2

matrixMultiply :: Num a => [[a]] -> [[a]] -> [[a]]
matrixMultiply m1 m2
  | m1Cols /= m2Rows = [[]]
  | m2Cols == 1 = let v = concat m2 in map ((\x -> [x]) . dotProduct v) m1
  | m1Cols == 1 = let v = concat m1 in [map (*x) (concat m2) | x <- v]
  | otherwise = [[dotProduct (m1 !! i) $ transpM2 !! j
                | j <- [0..(m2Cols - 1)]]
                | i <- [0..(m1Rows - 1)]]
  where m1Rows = length m1
        m1Cols = length (head m1)
        m2Rows = length m2
        m2Cols = length (head m2)
        transpM2 = transpose m2
