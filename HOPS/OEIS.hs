{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Copyright   : Anders Claesson 2015, 2016
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module HOPS.OEIS
    (
    -- * Types
      URL
    , ANum (..)
    , Sequence
    -- * Parse stripped.gz
    , parseStripped
    -- * Parse sequences
    , shave
    , parseIntegerSeq
    , parseIntegerSeqErr
    -- * Parse A-numbers and TAGs
    , aNumInt
    , tag
    ) where

import GHC.Generics (Generic)
import Data.Maybe
import Data.Monoid
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Aeson
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8
import Control.Monad
import Control.Applicative
import HOPS.Utils

-- | A sequence of rational numbers.
type Sequence = [Rational]

-- | A URL is currently just a synonym for `String`.
type URL = String

-- | An A-number is the character \'A\' followed by a six digit
-- number. Here we represent that by an Int
newtype ANum = ANum {unANum :: Int} deriving (Eq, Ord, Show, Generic)

instance ToJSON ANum where
    toJSON m = String ("A" <> decodeUtf8 (packANum m))

instance FromJSON ANum where
    parseJSON (String s) = pure $ parseANumErr (encodeUtf8 s)
    parseJSON _ = mzero

spc :: Parser Char
spc = char ' '

packANum :: ANum -> ByteString
packANum = B.pack . show . unANum

aNum :: Parser ANum
aNum = ANum <$> (char 'A' *> decimal)

parseANum :: ByteString -> Maybe ANum
parseANum = parse_ (aNum <* endOfInput)

parseANumErr :: ByteString -> ANum
parseANumErr = fromMaybe (error "error parsing A-number") . parseANum

-------------------------------------------------------------------------------
-- Parsing stripped.gz
-------------------------------------------------------------------------------

dropHeader :: [ByteString] -> [ByteString]
dropHeader = dropWhile (\line -> B.head line == '#')

parseRecords :: ByteString -> [(ANum, ByteString)]
parseRecords = mapMaybe (parse_ record) . dropHeader . B.lines
  where
    record = (,) <$> (aNum <* spc) <*> A.takeByteString

-- | Parse a list of A-number-sequence pairs. It's purpose is to parse
-- lines of the @stripped@ file. A typical line of that file looks like
-- this:
--
-- > A000108 ,1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900,
--
parseStripped :: ByteString -> [(ANum, Sequence)]
parseStripped bs = [ (anum, parseIntegerSeqErr (shave s)) | (anum, s) <- parseRecords bs ]

-------------------------------------------------------------------------------
-- Parse sequences
-------------------------------------------------------------------------------

integerSeq :: Parser Sequence
integerSeq =
    map (fromIntegral :: Integer -> Rational) <$> signed decimal `sepBy` char ','

-- | Parse a sequence of `Integer`s.
parseIntegerSeq :: ByteString -> Maybe Sequence
parseIntegerSeq = parse_ (integerSeq <* endOfInput) . B.filter (/=' ')

-- | Parse a sequence of `Integer`s or throw an error.
parseIntegerSeqErr :: ByteString -> Sequence
parseIntegerSeqErr = fromMaybe (error "error parsing sequence") . parseIntegerSeq

-------------------------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------------------------

-- | \"Shave\" off the first and last element of a `ByteString`. E.g.
--
-- > shave "{1,2,3}" = "1,2,3"
--
shave :: ByteString -> ByteString
shave = B.init . B.tail

-- | A parser for A-numbers as `Int`s.
aNumInt :: Parser Int
aNumInt = char 'A' >> decimal

-- | A parser for tags (B-numbers) as `Int`s.
tag :: Parser Int
tag = string "TAG" >> decimal
