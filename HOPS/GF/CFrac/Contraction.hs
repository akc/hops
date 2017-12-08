-- |
-- Copyright   : Anders Claesson 2017
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module HOPS.GF.CFrac.Contraction
    ( contraction
    ) where

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

-- The contraction formula relating S- and J-fractions:
--
-- S[c_k] = J[c_{2*k-1} + c_{2*k}, c_{2*k} * c_{2*k+1}]
--
contraction :: Fractional a => (Vector a -> Vector a) -> Vector a -> (Vector a, Vector a)
contraction sfrac cs =
    if V.null cs then
        (V.empty, V.empty)
    else
        (u, V.cons (cs ! 0) v)
  where
    ds = V.drop 1 $ sfrac cs
    n = V.length ds `div` 2
    f (-1) = 0
    f i = ds ! i
    u = V.fromList $ (\k -> f (2*k-1) + f (2*k)) <$> [0 .. n]
    v = V.fromList $ (\k -> f (2*k) * f (2*k+1)) <$> [0 .. n - 1]
