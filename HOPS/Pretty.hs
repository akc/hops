{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright   : Anders Claesson 2016
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module HOPS.Pretty (Pretty (..)) where

import Data.Ratio
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

-- Pretty printing
class Pretty a where
    pretty :: a -> ByteString

instance Pretty Int where
    pretty = B.pack . show

instance Pretty Integer where
    pretty = B.pack . show

instance (Pretty a, Integral a) => Pretty (Ratio a) where
    pretty r = B.concat [pretty (numerator r), "/", pretty (denominator r)]
