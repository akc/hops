{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Copyright   : Anders Claesson 2015, 2016
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module HOPS.DB
    ( DB (..)
    , readSeqDB
    , readANumDB
    , emptyANumDB
    ) where

import GHC.TypeLits
import Data.Proxy
import Data.Vector (Vector, (//))
import qualified Data.Vector as V
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import System.Directory
import HOPS.GF.Series
import HOPS.OEIS
import HOPS.Config
import Paths_hops

-- | A data base (DB) is just a wrapped `ByteString`.
newtype DB = DB {unDB :: ByteString} deriving Show

-- | Read the DB at the given location.
readDB :: FilePath -> IO DB
readDB fpath = doesFileExist fpath >>= \b ->
    DB <$> (B.readFile =<< if b then return fpath
                                else getDataFileName "data/stub.db")

-- | Read the sequence DB (derived from \"stripped.gz\").
readSeqDB :: Config -> IO DB
readSeqDB = readDB . seqDBPath

-- | Create a vector that at index 'n' contains the sequence with
-- A-number 'n'.
readANumDB :: KnownNat n => Config -> IO  (Vector (Series n))
readANumDB cfg = do
    entries <- parseStripped . unDB <$> readSeqDB cfg
    let series' = series (Proxy :: Proxy n) . map Val
    let pairs = [ (unANum a - 1, series' s) | (a, s) <- entries ]
    return $ V.replicate 350000 (series' []) // pairs

-- | An empty A-number database
emptyANumDB :: Vector (Series n)
emptyANumDB = V.empty
