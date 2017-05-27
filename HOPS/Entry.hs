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
    { getExpr  :: Expr
    , getSeq   :: Sequence
    , getTrail :: [PackedExpr]
    } deriving (Eq, Show)

instance ToJSON Entry where
    toJSON (Entry prg s trail) =
        object ([ "hops"  .= toJSON prg
                , "seq"   .= toJSON (map numerator s)
                ] ++
                [ "trail" .= toJSON trail | not (null trail) ] ++
                [ "denominators" .= toJSON ds
                | let ds = map denominator s
                , any (/=1) ds  -- For terseness only include denominators if
                                -- at least one of them isn't 1
                ]
               )

instance FromJSON Entry where
    parseJSON (Object v) = do
        prg <- v .:  "hops"
        ns  <- v .:  "seq"
        mds <- v .:? "denominators"
        mtr <- v .:? "trail"
        let tr = fromMaybe [] mtr
        let ds = fromMaybe (repeat 1) mds
        return $ Entry prg (zipWith (%) ns ds) tr
    parseJSON _ = mzero
