{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module HOPS.DB
    ( DB (..), Sequences
    , readSeqDB
    , readANumDB
    , emptyANumDB
    ) where

import Control.Applicative
import GHC.TypeLits
import Data.Proxy
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import System.Directory
import HOPS.GF.Series
import HOPS.OEIS
import HOPS.Config

-- | An empty data declaration used with the phantom `DB` data type.
data Sequences

-- | A data base (DB) is just a wrapped `ByteString`.
newtype DB a = DB {unDB :: ByteString} deriving Show

-- | Read the DB at the given location.
readDB :: FilePath -> IO (DB a)
readDB fpath = doesFileExist fpath >>= \b ->
    if b then DB <$> B.readFile fpath
         else error "No local A-number database; run 'hops --update' first."

-- | Read the sequence DB (derived from \"stripped.gz\").
readSeqDB :: Config -> IO (DB Sequences)
readSeqDB = readDB . seqDBPath

-- | Create a vector that at index 'n' contains the sequence with
-- A-number 'n'.
readANumDB :: KnownNat n => Config -> IO  (Vector (Series n))
readANumDB cfg =
    V.fromList . map mkSeries . parseStripped . unDB <$> readSeqDB cfg
  where
    mkSeries = series (Proxy :: Proxy n)
             . map fromIntegral
             . fromMaybe (error "cannot parse local database")
             . parseIntegerSeq . unPSeq . snd

-- | An empty A-number database
emptyANumDB :: KnownNat n => Vector (Series n)
emptyANumDB = V.empty
