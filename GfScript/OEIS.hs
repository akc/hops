{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module GfScript.OEIS
    (
    -- * Types
      URL
    , Name
    , ANum (..)
    , PackedSeq (..)
    -- * Parse stripped.gz
    , parseStripped
    -- * Parse sequences
    , shave
    , parseSeqErr
    , parseIntegerSeq
    , packedSeq
    , packSeq
    -- * Parse A-numbers and TAGs
    , aNumInt
    , tag
    ) where

import GHC.Generics (Generic)
import Data.Maybe
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
import Data.String
import Data.Ratio
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Aeson
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as Ch
import Data.Attoparsec.ByteString.Char8
import Control.DeepSeq
import Control.Monad
import Control.Applicative
import GfScript.Utils

-- | The name of an OEIS entry is a short description of the
-- sequence. Here represented as a `ByteString`.
type Name = ByteString

-- | A URL is currently just a synonym for `String`.
type URL  = String

-- | An A-number is the character \'A\' followed by a six digit
-- number. Here we represent that by a wrapped (7 character)
-- `ByteString`.
newtype ANum = ANum {unANum :: ByteString} deriving (Eq, Ord, Show, Generic)

-- | A `PackedSeq` is a wrapped `ByteString`.
newtype PackedSeq = PSeq {unPSeq :: ByteString} deriving (Eq, Show, Generic)

instance NFData PackedSeq

instance Monoid PackedSeq where
    mempty = PSeq mempty
    mappend (PSeq x) (PSeq y) = PSeq (mappend x y)

instance IsString PackedSeq where
    fromString = PSeq . fromString

instance ToJSON ANum where
    toJSON (ANum bs) = String (decodeUtf8 bs)

instance FromJSON ANum where
    parseJSON (String s) = pure $ ANum (encodeUtf8 s)
    parseJSON _ = mzero

instance ToJSON PackedSeq where
    toJSON (PSeq bs) = String (decodeUtf8 bs)

instance FromJSON PackedSeq where
    parseJSON (String s) = pure $ PSeq (encodeUtf8 s)
    parseJSON _ = mzero

spc :: Parser Char
spc = char ' '

aNum :: Parser ANum
aNum = ANum <$> (B.cons <$> char 'A' <*> takeWhile1 isDigit)

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
parseStripped :: ByteString -> [(ANum, PackedSeq)]
parseStripped bs = [ (anum, PSeq (shave s)) | (anum, s) <- parseRecords bs ]

-------------------------------------------------------------------------------
-- Parse sequences
-------------------------------------------------------------------------------

rat :: Parser Rational
rat = (%) <$> signed decimal <*> ((char '/' *> decimal) <|> return 1)

ratSeq :: Parser [Rational]
ratSeq = rat `sepBy` char ','

integerSeq :: Parser [Integer]
integerSeq = signed decimal `sepBy` char ','

parseSeq :: ByteString -> Maybe [Rational]
parseSeq = parse_ (ratSeq <* endOfInput) . B.filter (/=' ')

-- | Parse a sequence of `Rational`s.
parseSeqErr :: ByteString -> [Rational]
parseSeqErr = fromMaybe (error "error parsing sequence") . parseSeq

-- | Parse a sequence of `Integer`s.
parseIntegerSeq :: ByteString -> Maybe [Integer]
parseIntegerSeq = parse_ (integerSeq <* endOfInput) . B.filter (/=' ')

-- | Parser for `PackedSeq`.
packedSeq :: Parser PackedSeq
packedSeq = PSeq <$> (char '{' *> Ch.takeWhile (/='}') <* char '}')

-- | Pack a sequence of `Rational`s into a `PackedSeq`. E.g.
--
-- > packSeq [1,1/2,1/3] = PSeq {unPSeq = "1,1/2,1/3"}
--
packSeq :: [Rational] -> PackedSeq
packSeq = PSeq . B.intercalate (B.pack ",") . map (B.pack . f)
  where
    f r = case (numerator r, denominator r) of
            (n, 1) -> show n
            (n, d) -> show n ++ '/':show d

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
