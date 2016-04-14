-- |
-- Copyright   : Anders Claesson 2016
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module HOPS.Pretty (Pretty (..)) where

import Data.ByteString.Char8 (ByteString, pack)

-- Pretty printing
class Pretty a where
    pretty :: a -> ByteString


instance Pretty Integer where
    pretty = pack . show
