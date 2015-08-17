{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
-- Truncated power series with rational coefficients.
--
-- When writing this module I benefited from ideas and inspiration from
-- the following sources:
--
--     * J. Merrill "Truncated Power Series for Julia",
--       <https://github.com/jwmerrill/PowerSeries.jl>.
--     * M. D. McIlroy "Power serious: power series in ten one-liners",
--       <http://www1.cs.dartmouth.edu/~doug/powser.html>.
--     * D. Piponi (sigfpe) "A Small Combinatorial Library",
--       <http://blog.sigfpe.com/2007/11/small-combinatorial-library.html>.

module GfScript.Gf.Series
    ( module GfScript.Gf.Rat
    , Series (..)
    -- * Constructions
    , polynomial
    , series
    -- * Accessors
    , precision
    , coeffVector
    , coeffList
    , rationalPrefix
    , eval
    -- * Operations
    , (.*)
    , (./)
    , (!^!)
    , (^!)
    , o
    , derivative
    , integral
    , revert
    , sec
    , fac
    ) where

import GHC.TypeLits
import Data.Proxy
import Data.Ratio
import Data.Maybe
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import GfScript.Gf.Rat

-- | A truncated power series is represented as a (dense) vector of
-- coefficients. The precision (maximum number of coefficients) is
-- statically checked. For instance, adding two series of different
-- precision would result in a type error.
newtype Series (n :: Nat) = Series (Vector Rat)

instance Show (Series n) where
    showsPrec p f =
        let s = "series (Proxy :: Proxy " ++ show (precision f) ++ ") "
        in showParen (p > 10) $ showString s . shows (coeffList f)

instance Eq (Series n) where
    f == g = coeffVector f == coeffVector g

instance KnownNat n => Num (Series n) where
    (Series u) + (Series v) = Series $ V.zipWith (+) u v
    (Series u) - (Series v) = Series $ V.zipWith (-) u v
    (Series u) * (Series v) = Series $ u `mul` v
    fromInteger x = polynomial (Proxy :: Proxy n) [fromIntegral x]
    abs = undefined
    signum = undefined

instance KnownNat n => Fractional (Series n) where
    fromRational r = polynomial (Proxy :: Proxy n) [fromRational r]
    (Series u) / (Series v) = Series $ u `divide` v

instance KnownNat n => Floating (Series n) where
    pi = polynomial (Proxy :: Proxy n) [pi]
    exp = exp'
    log f = c0 log f + integral (derivative f / restrict f)
    f ** g = if isConstant g then f ^! constant g else exp (g * log f)
    sin f = c0 sin f + integral (derivative f * cos (restrict f))
    cos f = c0 cos f - integral (derivative f * sin (restrict f))
    asin f = c0 asin f + integral (derivative f / sqrt (1 - restrict f^(2::Int)))
    acos f = c0 acos f - integral (derivative f / sqrt (1 - restrict f^(2::Int)))
    atan f = c0 atan f + integral (derivative f / (1 + restrict f^(2::Int)))
    sinh f = c0 sinh f + integral (derivative f * cosh (restrict f))
    cosh f = c0 cosh f + integral (derivative f * sinh (restrict f))
    asinh f = c0 asinh f + integral (derivative f / sqrt (restrict f^(2::Int) + 1))
    acosh f = c0 acosh f + integral (derivative f / sqrt (restrict f^(2::Int) - 1))
    atanh f = c0 atanh f + integral (derivative f / (1 - restrict f^(2::Int)))
    sqrt = squareRoot

-- | The underlying vector of coefficients. E.g.
--
-- >>> coeffVector $ polynomial (Proxy :: Proxy 3) [1,2]
-- fromList [Val (1 % 1),Val (2 % 1),Val (0 % 1)]
--
coeffVector :: Series n -> Vector Rat
coeffVector (Series v) = v

-- | The list of coefficients of the given series. E.g.
--
-- >>> coeffList $ series (Proxy :: Proxy 3) [9]
-- [Val (9 % 1),Indet,Indet]
--
coeffList :: Series n -> [Rat]
coeffList = V.toList . coeffVector

-- | The longest initial segment of coefficients that are rational
-- (i.e. not `Indet` or `DZ`).
rationalPrefix :: Series n -> [Rational]
rationalPrefix = mapMaybe maybeRational . takeWhile isRational . coeffList

-- | If @f :: Series n@, then @precision f = n@.
precision :: Series n -> Int
precision (Series v) = V.length v

-- | Create a polynomial with the given list of coefficients. E.g.
--
-- >>> (polynomial (Proxy :: Proxy 4) [1,1])^2
-- series (Proxy :: Proxy 4) [Val (1 % 1),Val (2 % 1),Val (1 % 1),Val (0 % 1)]
--
polynomial :: KnownNat n => Proxy n -> [Rat] -> Series n
polynomial n xs = Series $ V.fromListN (fromInteger (natVal n)) (xs ++ repeat 0)

-- | Create a power series with the given list of initial
-- coefficients. E.g.
--
-- >>> (series (Proxy :: Proxy 4) [1,1])^2
-- series (Proxy :: Proxy 4) [Val (1 % 1),Val (2 % 1),Indet,Indet]
--
series :: KnownNat n => Proxy n -> [Rat] -> Series n
series n xs = Series $ V.fromListN (fromInteger (natVal n)) (xs ++ repeat Indet)

-- | Coefficient wise multiplication of two power series. Also called
-- the Hadamard product.
(.*) :: Series n -> Series n -> Series n
(.*) (Series u) (Series v) = Series $ V.zipWith (*) u v

-- | Coefficient wise division of two power series.
(./) :: Series n -> Series n -> Series n
(./) (Series u) (Series v) = Series $ V.zipWith (/) u v

mul :: Vector Rat -> Vector Rat -> Vector Rat
mul u v = V.generate (V.length u) $ \n -> sum [ u!i * v!(n-i) | i <- [0..n] ]

divide :: Vector Rat -> Vector Rat -> Vector Rat
divide u v | V.null v || V.null u = V.empty
divide u v =
    let u' = V.tail u
        v' = V.tail v
    in case (V.head u, V.head v) of
        (0, 0) -> V.snoc (divide u' v') Indet
        (c, d) -> let q   = c/d
                      qv' = V.map (q*) v'
                  in V.cons q $ divide (V.zipWith (-) u' qv') (V.init v)

-- | The (formal) derivative of a power series.
derivative :: Series n -> Series n
derivative (Series v) = Series $ V.snoc u Indet
  where
    u = V.imap (\i a -> a * fromIntegral (i+1)) (V.tail v)

-- | The (formal) integral of a power series.
integral :: Series n -> Series n
integral (Series v) = Series (V.cons 0 u)
  where
    u = V.imap (\i a -> a / fromIntegral (i+1)) (V.init v)

-- | Reversion (compositional inverse) of a power series.  Unless the
-- constant of the power series is zero the result is indeterminate.
--
-- >>> revert $ series (Proxy :: Proxy 4) [0,1,2,3]
-- series (Proxy :: Proxy 4) [Val (0 % 1),Val (1 % 1),Val ((-2) % 1),Val (5 % 1)]
--
-- >>> revert $ series (Proxy :: Proxy 4) [1,1,1,1]
-- series (Proxy :: Proxy 4) [Indet,Indet,Indet,Indet]
--
revert :: Series n -> Series n
revert (Series u) = Series (rev u)
  where
    rev v | V.null v      = V.empty
          | V.head v /= 0 = V.fromListN n (repeat Indet)
          | otherwise     = iterate f ind !! n
        where
          n   = V.length v
          ind = V.fromListN (n-1) (repeat Indet)
          one = V.fromListN (n-1) (1 : repeat 0)
          f w = V.cons 0 $ one `divide` (V.tail v `comp` w)

-- | Evaluate the polynomial @p@ at @x@ using Horner's method.
--
-- >>> let x = polynomial (Proxy :: Proxy 5) [0,1]
-- >>> eval (1-x+x^2) 2
-- Val (3 % 1)
--
eval :: Series n -> Rat -> Rat
eval (Series v) x = V.foldr (\c s -> x * s + c) 0 v

comp :: Vector Rat -> Vector Rat -> Vector Rat
comp _ v | V.null v = V.empty
comp u v =
    case V.head v of
      0 -> V.cons c (v' `mul` (u' `comp` w))
      _ -> V.imap addc (v `mul` V.snoc (u' `comp` w) 0)
    where
      addc 0 a = a + c
      addc _ a = a
      u' = V.tail u
      v' = V.tail v
      c  = V.head u
      w  = V.init v

infixr 7 `o`

-- | The composition of two power series.
--
-- >>> let x = polynomial (Proxy :: Proxy 4) [0,1]
-- >>> (1/(1-x)) `o` (2*x)
-- series (Proxy :: Proxy 4) [Val (1 % 1),Val (2 % 1),Val (4 % 1),Val (8 % 1)]
--
o :: KnownNat n => Series n -> Series n -> Series n
o (Series u) (Series v) = Series $ u `comp` v

c0 :: KnownNat n => (Rat -> Rat) -> Series n -> Series n
c0 f (Series v) = fromRat $ f (V.head v)

constant :: Series n -> Rat
constant (Series v) = V.head v

fromRat :: KnownNat n => Rat -> Series n
fromRat c = polynomial (Proxy :: Proxy n) [c]

kRestrict :: Int -> Series n -> Series n
kRestrict k (Series v) = Series (v // [(k, Indet)])

restrict :: Series n -> Series n
restrict f = kRestrict (precision f - 1) f

exp' :: KnownNat n => Series n -> Series n
exp' f = expAux (precision f - 1) f
  where
    expAux 0 _ = 1
    expAux d g =
        let h = expAux (d-1) (kRestrict d g)
        in c0 exp g + integral (derivative g * h)

-- | The power operator for `Rat`s. E.g.
--
-- >>> (1/4) !^! (3/2)
-- Val (1 % 8)
--
(!^!) :: Rat -> Rat -> Rat
(!^!) c r = let Series v = polynomial (Proxy :: Proxy 2) [c] ^! r in V.head v

-- | A power series raised to a rational power.
--
-- >>> series (Proxy :: Proxy 4) [1,2,3,4] ^! (1/2)
-- series (Proxy :: Proxy 4) [Val (1 % 1),Val (1 % 1),Val (1 % 1),Val (1 % 1)]
--
(^!) :: KnownNat n => Series n -> Rat -> Series n
(^!) _ DZ = polynomial (Proxy :: Proxy n) [DZ]
(^!) _ Indet = polynomial (Proxy :: Proxy n) [Indet]
(^!) f s@(Val r) =
    case (numerator r, denominator r) of
      (0, _) -> polynomial (Proxy :: Proxy n) [1]
      (n, _) | n < 0 -> 1 / (f ^! (-s))
      (n, 1) -> f ^^ n
      (n, 2) -> squareRoot f ^^ n
      (_, _) -> exp' (fromRational r * log f)

isConstant :: KnownNat n => Series n -> Bool
isConstant (Series u) = V.all (==0) (V.tail u)

squareRoot :: KnownNat n => Series n -> Series n
squareRoot f = squareRoot' (precision f - 1) f
  where
    squareRoot' 0 _ = 1
    squareRoot' d g =
        let h = 2 * squareRoot' (d-1) (kRestrict d g)
        in c0 sqrt g + integral (derivative g / h)

-- | The secant function: @sec f = 1 / cos f@
sec :: KnownNat n => Series n -> Series n
sec f = 1 / cos f

-- | The factorial of a constant power series. If the the power series
-- isn't constant, then the result is indeterminate (represented using
-- `Indet`).
--
-- >>> fac (polynomial (Proxy :: Proxy 4) [3])
-- series (Proxy :: Proxy 4) [Val (6 % 1),Val (0 % 1),Val (0 % 1),Val (0 % 1)]
--
fac :: KnownNat n => Series n -> Series n
fac f | isConstant f = polynomial (Proxy :: Proxy n) [factorial (constant f)]
      | otherwise    = polynomial (Proxy :: Proxy n) [Indet]
