{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright   : Anders Claesson 2015, 2016
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module HOPS.Entry
    ( Sequence
    , Entry (..)
    ) where

import Data.Ratio
import Data.Maybe
import Data.Aeson
import Control.Monad
import HOPS.GF
import HOPS.OEIS

-- | An entry consists of a program together with a list of rational
-- numbers.
data Entry = Entry
    { getPrg   :: Prg Integer
    , getSeq   :: Sequence
    , getTrail :: [PackedPrg]
    } deriving (Eq, Show)

instance ToJSON Entry where
    toJSON (Entry prg s trail) =
        object ([ "hops"  .= toJSON prg
                , "seq"   .= toJSON (map numerator s)
                , "trail" .= toJSON trail
                ] ++
                [ "denominators" .= toJSON ds
                | let ds = map denominator s
                , any (/=1) ds  -- For terseness only include denominators if
                                -- at least one of them isn't 1
                ]
               )

instance FromJSON Entry where
    parseJSON (Object v) = do
        prg   <- v .:  "hops"
        ns    <- v .:  "seq"
        mds   <- v .:? "denominators"
        trail <- v .:  "trail"
        let ds = fromMaybe (repeat 1) mds
        return $ Entry prg (zipWith (%) ns ds) trail
    parseJSON _ = mzero
