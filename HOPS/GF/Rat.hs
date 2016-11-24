{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
-- Rational numbers extended with two elements representing a finite but
-- indeterminate value and divison by zero, respectively.

module HOPS.GF.Rat
    ( Rat (..)
    , maybeRational
    , maybeInteger
    , maybeInt
    , isRational
    , isInteger
    , isInt
    , factorial
    , binomial
    , choose
    , multinomial
    ) where

import Data.Ratio
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B
import HOPS.Pretty

-- | Rationals extended with two elements:
data Rat
    -- | A known rational value.
    = Val {-# UNPACK #-} !Rational
    -- | An unknown value.
    | Indet
    -- | Division by zero.
    | DZ
      deriving (Eq, Ord, Show, Read)

-- | Addition and multiplication tables:
--
-- @
--
--    +  | Val b     | Indet | DZ
-- ------+-----------+-------+----
-- Val a | Val (a+b) | Indet | DZ
-- Indet | Indet     | Indet | DZ
-- DZ    | DZ        | DZ    | DZ
--
--    *  | Val 0 | Val b     | Indet | DZ
-- ------+-------+-----------+-------+----
-- Val 0 | Val 0 | Val 0     | Val 0 | DZ
-- Val a | Val 0 | Val (a*b) | Indet | DZ
-- Indet | Val 0 | Indet     | Indet | DZ
-- DZ    | DZ    | DZ        | DZ    | DZ
-- @
--
instance Num Rat where
    (Val x) + (Val y) = Val (x+y)
    DZ      + _       = DZ
    _       + DZ      = DZ
    Indet   + _       = Indet
    _       + Indet   = Indet
    {-# INLINE (+) #-}

    (Val x) - (Val y) = Val (x-y)
    DZ      - _       = DZ
    _       - DZ      = DZ
    Indet   - _       = Indet
    _       - Indet   = Indet
    {-# INLINE (-) #-}

    (Val x) * (Val y) = Val (x*y)
    (Val 0) * Indet   = 0
    (Val _) * Indet   = Indet
    _       * DZ      = DZ
    DZ      * _       = DZ
    Indet   * (Val 0) = 0
    Indet   * (Val _) = Indet
    Indet   * Indet   = Indet
    {-# INLINE (*) #-}

    negate (Val x) = Val (negate x)
    negate Indet   = Indet
    negate DZ      = DZ
    {-# INLINE negate #-}

    abs (Val x) = Val (abs x)
    abs Indet   = Indet
    abs DZ      = DZ
    {-# INLINE abs #-}

    signum (Val x) = Val (signum x)
    signum Indet   = Indet
    signum DZ      = DZ
    {-# INLINE signum #-}

    fromInteger i = Val (fromInteger i)
    {-# INLINE fromInteger #-}

-- | Division table:
--
-- @
--
--    /  | Val 0 | Val b     | Indet | DZ
-- ------+-------+-----------+-------+----
-- Val 0 | DZ    | Val 0     | Val 0 | DZ
-- Val a | DZ    | Val (a/b) | Indet | DZ
-- Indet | DZ    | Indet     | Indet | DZ
-- DZ    | DZ    | DZ        | DZ    | DZ
-- @
--
instance Fractional Rat where
    _       / (Val 0) = DZ
    (Val 0) / Indet   = Val 0
    (Val x) / (Val y) = Val (x/y)
    (Val _) / Indet   = Indet
    DZ      / _       = DZ
    _       / DZ      = DZ
    Indet   / _       = Indet
    {-# INLINE (/) #-}

    fromRational = Val . fromRational
    {-# INLINE fromRational #-}

instance Floating Rat where
    pi = Val (toRational (pi :: Double))
    exp = lift exp
    log = lift log
    sin = lift sin
    cos = lift cos
    asin = lift asin
    acos = lift acos
    atan = lift atan
    sinh = lift sinh
    cosh = lift cosh
    asinh = lift asinh
    acosh = lift acosh
    atanh = lift atanh
    sqrt DZ = DZ
    sqrt Indet = Indet
    sqrt (Val r) =
        let p = toRational $ sqrt (fromInteger (numerator r) :: Double)
            q = toRational $ sqrt (fromInteger (denominator r) :: Double)
        in Val (p/q)

instance Pretty Rat where
    pretty DZ = "DZ"
    pretty Indet = "Indet"
    pretty (Val r) = B.concat [pretty (numerator r), "/", pretty (denominator r)]

lift :: (Double -> Double) -> Rat -> Rat
lift f (Val r) = Val $ toRational $ f (fromRational r)
lift _ Indet = Indet
lift _ DZ = DZ
{-# INLINE lift #-}

-- | Is the given element a rational?
isRational :: Rat -> Bool
isRational (Val _) = True
isRational _       = False
{-# INLINE isRational #-}

-- | Is the given element an Int?
isInt :: Rat -> Bool
isInt (Val r) | denominator r == 1 =
    let i = numerator r
        minInt = toInteger (minBound :: Int)
        maxInt = toInteger (maxBound :: Int)
    in minInt <= i && i <= maxInt
isInt _  = False

-- | Is the given element an Integer?
isInteger :: Rat -> Bool
isInteger (Val r) | denominator r == 1 = True
isInteger _ = False

-- | `maybeRational` of @Val x@ is @Just x@, otherwise it is `Nothing`.
maybeRational :: Rat -> Maybe Rational
maybeRational (Val x) = Just x
maybeRational _       = Nothing

-- | `maybeInteger` of @Val x@ is `Just` the numerator of @x@ if the
-- denominator of @x@ is 1, otherwise it is `Nothing`.
maybeInteger :: Rat -> Maybe Integer
maybeInteger (Val r) | denominator r == 1 = Just (numerator r)
maybeInteger _ = Nothing
{-# INLINE maybeInteger #-}

-- | Like `maybeInteger` but return `Nothing` when the integer is out of
-- range for an `Int`.
maybeInt :: Rat -> Maybe Int
maybeInt x@(Val r) | isInt x = Just (fromInteger (numerator r))
maybeInt _ = Nothing
{-# INLINE maybeInt #-}

-- | If the given value represents a nonnegative integer, then the
-- factorial of that integer is returned. If given `DZ`, return `DZ`. In
-- all other cases return `Indet`.
factorial :: Rat -> Rat
factorial DZ = DZ
factorial r  =
    case maybeInteger r of
      Nothing            -> Indet
      Just k | k < 0     -> Indet
             | otherwise -> Val $ toRational $ product [1 .. k]

pascal :: [Vector Integer]
pascal = [ V.fromList [ n `binomial` k | k <- [0 .. (n+1) `div` 2 ] ] | n <- [0..] ]

binomial :: Int -> Int -> Integer
n `binomial` k
    | n < k            = 0
    | k == 0           = 1
    | n == k           = 1
    | k < n && 2*k > n = n `binomial` (n-k)
    | otherwise        = pascal!!(n-1)!k  + pascal!!(n-1)!(k-1)

choose :: (Integral a, Num b) => a -> a -> b
choose n k = fromInteger $ binomial (fromIntegral n) (fromIntegral k)

multinomial :: (Integral b, Fractional a) => [b] -> a
multinomial [] = 1
multinomial [_] = 1
multinomial [k0,k1] = (k0+k1) `choose` k0
multinomial [k0,k1,k2] = (k0+k1+k2) `choose` k0 * (k1+k2) `choose` k1
multinomial ks@(k:kt) = fromIntegral (sum ks) `choose` k * multinomial kt
