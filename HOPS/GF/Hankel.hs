-- |
-- Copyright   : Anders Claesson 2017
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module HOPS.GF.Hankel
    ( hankelMatrix
    , hankel
    , hankel1
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import HOPS.GF.Rat
import HOPS.Utils.Matrix (det)

hankelMatrix :: Vector Rat -> Vector (Vector Rat)
hankelMatrix v = V.iterateN (V.length v) (\u -> V.snoc (V.tail u) Indet) v

hankel :: Vector Rat -> Vector Rat
hankel v = V.reverse $ V.map det subMatrices
  where
    n = V.length v
    subMatrices = V.iterateN n (V.init . V.map V.init) (hankelMatrix v)

hankel1 :: Vector Rat -> Vector Rat
hankel1 = (`V.snoc` Indet) . hankel . V.tail
