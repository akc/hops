-- |
-- Copyright   : Anders Claesson 2017
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

-- Quotient difference algorithm for S- and J-fractions

module HOPS.GF.CFrac.QD
    ( stieltjes
    , jacobi
    , jacobi0
    , jacobi1
    ) where

import Data.Function.Memoize
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import HOPS.GF.CFrac.Contraction

jacobi0 :: Fractional a => Vector a -> Vector a
jacobi0 = fst . jacobi

jacobi1 :: Fractional a => Vector a -> Vector a
jacobi1 = snd . jacobi

-- XXX: Jacobi0/1 is currently broken:
--
-- *Main> quickCheck (prop_Jacobi0_QD :: Series 3 -> Bool)
-- *** Failed! (after 6 tests):
-- Exception:
--   ./Data/Vector/Generic.hs:245 ((!)): index out of bounds (2,2)
--   CallStack (from HasCallStack):
--     error, called at .<snip>
-- series (Proxy :: Proxy 3) [Val (1 % 1),Val ((-3) % 4),Val (5 % 1)]

jacobi :: Fractional a => Vector a -> (Vector a, Vector a)
jacobi = contraction stieltjes

-- S-fraction using the Quotient-Difference Algorithm
stieltjes :: Fractional a => Vector a -> Vector a
stieltjes cs =
    if V.null cs then
        V.empty
    else
        V.fromList (cs ! 0 : twine qs es)
  where
    (m, m') =
        case V.length cs of
          n | odd n -> let k = (n-1) `div` 2 in (k, k)
            | otherwise -> let k = n `div` 2 in (k, k-1)

    qs = map (q 0) [ 1 .. m  ]
    es = map (e 0) [ 1 .. m' ]

    qM = memoize2 q
    eM = memoize2 e

    e _ 0 = 0
    e k j = eM (k+1) (j-1) + qM (k+1) j - qM k j

    q k 1 = cs ! (k+1) / cs ! k
    q k j = qM (k+1) (j-1) * eM (k+1) (j-1) / eM k (j-1)

twine :: [a] -> [a] -> [a]
twine [] bs = bs
twine (a:as) bs = a : twine bs as
