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
import Data.Aeson
import Control.Monad
import Control.Applicative
import HOPS.GF
import HOPS.OEIS

-- | An entry consists of a program together with a list of rational
-- numbers.
data Entry = Entry
    { getPrg :: Prg Integer
    , getSeq :: Sequence
    } deriving (Eq, Show)

instance ToJSON Entry where
    toJSON (Entry prg s) =
        object ([ "hops" .= toJSON prg
                , "nums" .= toJSON (map numerator s)
                ] ++
                [ "dnos" .= toJSON ds
                | let ds = map denominator s
                , any (/=1) ds  -- For terseness only include denominators if
                                -- at least one of them isn't 1
                ])

instance FromJSON Entry where
    parseJSON (Object v) = do
        prg <- v .:  "hops"
        ns  <- v .:  "nums"
        mds <- v .:? "dnos"
        return $ case mds of
             Nothing -> Entry prg (map fromIntegral ns)
             Just ds -> Entry prg (zipWith (%) ns ds)
    parseJSON _ = mzero
