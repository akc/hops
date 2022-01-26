-- |
-- Copyright   : Anders Claesson
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
-- Constants determined at runtime such as the home directory.

module HOPS.Config
    ( Config (..)
    , getConfig
    ) where

import System.FilePath ((</>))
import System.Directory

-- | A data type holding "constants" determined at runtime.
data Config = Config
    {
    -- | The home directory
      home      :: FilePath
    -- | Path to the '.hops' directory.
    , hopsDir   :: FilePath
    -- | Path to 'stripped' file.
    , seqDBPath :: FilePath
    }

-- | Get configuration.
getConfig :: IO Config
getConfig = do
    h <- getHomeDirectory
    let c = Config { home      = h
                   , hopsDir   = h </> ".oeis-data"
                   , seqDBPath = hopsDir c </> "stripped"
                   }
    return c
