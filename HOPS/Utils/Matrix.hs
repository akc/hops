-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
-- Bare-bones matrix library; just enough to compute determinants.

module HOPS.Utils.Matrix
    ( Matrix
    , matrix
    , det
    ) where

import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

-- | A Matrix is represented as a vector of vectors.
type Matrix a = Vector (Vector a)

-- | Construct a matrix from a list of rows.
matrix :: [[a]] -> Matrix a
matrix = V.fromList . map V.fromList

findPivot :: (Eq a, Num a) => Matrix a -> Maybe Int
findPivot = V.findIndex (\row ->  V.head row /= 0)

swapRows :: Int -> Int -> Matrix a -> Matrix a
swapRows i j m = m // [(i, m!j), (j, m!i)]

reduceHead :: Fractional a => Vector a -> Vector a -> Vector a
reduceHead u v = V.tail $ V.zipWith (-) v (V.map (*c) u)
    where
      c = V.head v / V.head u

reduceFirstColumn :: Fractional a => Matrix a -> (a, Matrix a)
reduceFirstColumn m = (V.head u, V.map (reduceHead u) (V.tail m))
    where
      u = V.head m

-- | Matrix determinant. It is assumed that the matrix is square.
det :: (Eq a, Fractional a) => Matrix a -> a
det m | V.null m  = 1
      | otherwise =
          case findPivot m of
            Nothing -> 0
            Just i  -> c * s * det m''
              where
                (s, m' ) = if i == 0 then (1, m) else (-1, swapRows i 0 m)
                (c, m'') = reduceFirstColumn m'
