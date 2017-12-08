-- |
-- Copyright   : Anders Claesson 2017
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module HOPS.GF.CFrac.Hankel
    ( stieltjes
    , jacobi
    , jacobi0
    , jacobi1
    ) where

import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import HOPS.GF.Rat
import HOPS.GF.Hankel
import HOPS.GF.CFrac.Contraction

-- S-fraction using the Hankel determinants
stieltjes :: Vector Rat -> Vector Rat
stieltjes v =
    V.generate (V.length v) f
  where
    h = hankel v
    h1 = hankel1 v
    f 0 = v!0
    f 1 = v!1/v!0
    f 2 = v!2/v!1 - f 1
    f i | even i    = let m = i `div` 2 in h!m * h1!(m-2) / (h!(m-1) * h1!(m-1))
        | otherwise = let m = (i-1) `div` 2 in h1!m * h!(m-1) / (h1!(m-1) * h!m)

jacobi :: Vector Rat -> Maybe (Vector Rat, Vector Rat)
jacobi y =
    if V.all (/=0) h && V.all (/=0) h1 then
        Just (u, v)
    else
        Nothing
  where
    w = V.map (\x -> x / (y!0)) y
    h = V.cons 1 $ hankel w
    h1 = V.cons 1 $ hankel1 w
    (n, n') =
        case V.length w of
          m | even m    -> let n = m `div` 2 in (n, n)
            | otherwise -> let n = (m-1) `div` 2 in (n, n+1)
    u = V.generate n  (\i -> f (i+1))
    v = V.generate n' (\i -> g (i+1))
    f 1 = w!1
    f n = (h!(n-1)*h1!n/h!n + h!n*h1!(n-2)/h!(n-1))/h1!(n-1)
    g 1 = y!0
    g n = h!n*h!(n-2)/(h!(n-1))^2

jacobi0 :: Vector Rat -> Maybe (Vector Rat)
jacobi0 = fmap fst . jacobi

jacobi1 :: Vector Rat -> Maybe (Vector Rat)
jacobi1 = fmap snd . jacobi

-- jacobi :: Vector Rat -> (Vector Rat, Vector Rat)
-- jacobi = contraction stieltjes
