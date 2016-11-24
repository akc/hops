{-# LANGUAGE OverloadedStrings #-}
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

module HOPS.GF.Series
    ( module HOPS.GF.Rat
    , Series (..)
    -- * Constructions
    , polynomial
    , series
    , xpow
    , nil
    , infty
    -- * Accessors
    , precision
    , coeffVector
    , coeffList
    , constant
    , leadingCoeff
    , rationalPrefix
    , integerPrefix
    , intPrefix
    , eval
    -- * Operations
    , (.*)
    , (./)
    , (!^!)
    , (^!)
    , (?)
    , o
    , blackDiamond
    , derivative
    , integral
    , revert
    , sec
    , fac
    , rseq
    , rseq'
    ) where

import GHC.TypeLits
import Data.Proxy
import Data.Ratio
import Data.Maybe
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B
import HOPS.GF.Rat
import HOPS.Pretty

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
    {-# INLINE (==) #-}

instance KnownNat n => Num (Series n) where
    (Series u) + (Series v) = Series $ V.zipWith (+) u v
    {-# INLINE (+) #-}
    (Series u) - (Series v) = Series $ V.zipWith (-) u v
    {-# INLINE (-) #-}
    (Series u) * (Series v) = Series $ u `mul` v
    {-# INLINE (*) #-}
    fromInteger x = polynomial (Proxy :: Proxy n) [fromIntegral x]
    {-# INLINE fromInteger #-}
    abs f = signum f * f
    {-# INLINE abs #-}
    signum f = polynomial (Proxy :: Proxy n) [signum (leadingCoeff f)]
    {-# INLINE signum #-}

instance KnownNat n => Fractional (Series n) where
    fromRational r = polynomial (Proxy :: Proxy n) [fromRational r]
    {-# INLINE fromRational #-}
    (Series u) / (Series v) = Series $ u `divide` v
    {-# INLINE (/) #-}

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
    sqrt f = f ^! (1/2)

instance Pretty (Series n) where
    pretty f = B.concat ["{", B.intercalate "," (map pretty (coeffList f)), "}"]

-- | The underlying vector of coefficients. E.g.
--
-- >>> coeffVector $ polynomial (Proxy :: Proxy 3) [1,2]
-- fromList [Val (1 % 1),Val (2 % 1),Val (0 % 1)]
--
coeffVector :: Series n -> Vector Rat
coeffVector (Series v) = v
{-# INLINE coeffVector #-}

-- | The list of coefficients of the given series. E.g.
--
-- >>> coeffList $ series (Proxy :: Proxy 3) [9]
-- [Val (9 % 1),Indet,Indet]
--
coeffList :: Series n -> [Rat]
coeffList = V.toList . coeffVector
{-# INLINE coeffList #-}

-- | The first nonzero coefficient when read from smaller to larger
-- powers of x. If no such coefficient exists then return 0.
leadingCoeff :: Series n -> Rat
leadingCoeff f =
    case dropWhile (==0) (coeffList f) of
      []    -> 0
      (c:_) -> c
{-# INLINE leadingCoeff #-}

-- | The longest initial segment of coefficients that are rational
-- (i.e. not `Indet` or `DZ`).
rationalPrefix :: Series n -> [Rational]
rationalPrefix = mapMaybe maybeRational . takeWhile isRational . coeffList
{-# INLINE rationalPrefix #-}

-- | The longest initial segment of coefficients that are integral
integerPrefix :: Series n -> [Integer]
integerPrefix = mapMaybe maybeInteger . takeWhile isInteger . coeffList
{-# INLINE integerPrefix #-}

-- | The longest initial segment of coefficients that are `Int`s
intPrefix :: Series n -> [Int]
intPrefix = mapMaybe maybeInt . takeWhile isInt . coeffList
{-# INLINE intPrefix #-}

-- | If @f :: Series n@, then @precision f = n@.
precision :: Series n -> Int
precision (Series v) = V.length v
{-# INLINE precision #-}

-- | Create a polynomial with the given list of coefficients. E.g.
--
-- >>> (polynomial (Proxy :: Proxy 4) [1,1])^2
-- series (Proxy :: Proxy 4) [Val (1 % 1),Val (2 % 1),Val (1 % 1),Val (0 % 1)]
--
polynomial :: KnownNat n => Proxy n -> [Rat] -> Series n
polynomial n xs = Series $ V.fromListN (fromInteger (natVal n)) (xs ++ repeat 0)
{-# INLINE polynomial #-}

-- | Create a power series with the given list of initial
-- coefficients. E.g.
--
-- >>> (series (Proxy :: Proxy 4) [1,1])^2
-- series (Proxy :: Proxy 4) [Val (1 % 1),Val (2 % 1),Indet,Indet]
--
series :: KnownNat n => Proxy n -> [Rat] -> Series n
series n xs = Series $ V.fromListN (fromInteger (natVal n)) (xs ++ repeat Indet)
{-# INLINE series #-}


-- | Create an empty power series. All its coefficients are `Indet`.
nil :: KnownNat n => Series n
nil = series (Proxy :: Proxy n) []

-- | A series whose constant term is `DZ`.
infty :: KnownNat n => Series n
infty = series (Proxy :: Proxy n) [DZ]

-- | Create the power series x^k.
xpow :: KnownNat n => Int -> Series n
xpow k = polynomial (Proxy :: Proxy n) $ replicate k 0 ++ [1]

-- | Coefficient wise multiplication of two power series. Also called
-- the Hadamard product.
(.*) :: Series n -> Series n -> Series n
(.*) (Series u) (Series v) = Series $ V.zipWith (*) u v
{-# INLINE (.*) #-}

-- | Coefficient wise division of two power series.
(./) :: Series n -> Series n -> Series n
(./) (Series u) (Series v) = Series $ V.zipWith (/) u v
{-# INLINE (./) #-}

mul :: Vector Rat -> Vector Rat -> Vector Rat
mul u v =
    let n = V.length u
    in if n < 17
       then convolution n u v
       else let mu = V.mapM maybeInt u
                mv = V.mapM maybeInt v
            in case (mu, mv) of
                (Just u', Just v') ->
                    V.fromList $ map fromIntegral (pmult n (V.toList u') (V.toList v'))
                _ -> convolution n u v

convolution :: Int -> Vector Rat -> Vector Rat -> Vector Rat
convolution n u v = V.generate n $ \j -> sum [u!i * v!(j-i) | i <- [0..j]]
{-# INLINE convolution #-}

peval :: [Int] -> Integer -> Integer
peval p x = foldr (\c s -> x * s + fromIntegral c) 0 p
{-# INLINE peval #-}

maxNorm :: [Int] -> Integer
maxNorm = maximum . (0:) . map (abs . toInteger)
{-# INLINE maxNorm #-}

-- Bound the largest absolute value of any coefficient in the product.
bound :: Int -> [Int] -> [Int] -> Integer
bound prec p q =
    let b = max 1 (toInteger prec * maxNorm p * maxNorm q)
    in 2^((2 :: Integer) + ceiling (logBase 2 (fromIntegral b) :: Double))
{-# INLINE bound #-}

-- Fast polynomial multiplication using Kronecker substitution. This
-- implementation is based on "Can we save time in multiplying
-- polynomials by encoding them as integers?" by R. J. Fateman (2010).
pmult :: Int -> [Int] -> [Int] -> [Integer]
pmult prec p q =
    take prec $ unpack (peval p a * peval q a) ++ repeat 0
  where
    a = bound prec p q
    unpack i = if i < 0 then map (*(-1)) $ unp (-i) else unp i
    unp 0 = []
    unp i = let (c,r) = quotRem i a
            in if 2*r > a
               then (r - a) : unp (c+1)
               else r       : unp c

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
revert (Series u) = Series rev
  where
    rev | V.null u      = V.empty
        | V.head u /= 0 = V.fromListN n (repeat Indet)
        | otherwise     = iterate f ind !! n
      where
        n   = V.length u
        ind = V.fromListN n (repeat Indet)
        one = V.fromListN (n-1) (1 : repeat 0)
        u'  = V.tail u
        f w = V.cons 0 $ one `divide` (u' `comp` V.init w)

-- | Evaluate the polynomial @p@ at @x@ using Horner's method.
--
-- >>> let x = polynomial (Proxy :: Proxy 5) [0,1]
-- >>> eval (1-x+x^2) 2
-- Val (3 % 1)
--
eval :: Series n -> Rat -> Rat
eval (Series v) x = V.foldr (\c s -> x * s + c) 0 v
{-# INLINE eval #-}

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

dropTrailing :: Eq a => a -> [a] -> [a]
dropTrailing x = reverse . dropWhile (== x) . reverse

-- | Construct a series that has coefficient 1 for each term whose power is
-- some coefficient of the input series and 0 elsewhere.  Elements of the input
-- series that are not nonnegative integers or not less than the precision are
-- ignored; trailing zeros are also ignored.
--
-- >>> rseq $ series (Proxy :: Proxy 4) [1,3]
--series (Proxy :: Proxy 4) [Val (0 % 1),Val (1 % 1),Val (0 % 1),Val (1 % 1)]
--
rseq :: Series n -> Series n
rseq f = Series $ V.replicate (precision f) 0 // zip cs (repeat 1)
  where
    cs = dropTrailing 0 [ x | x <- intPrefix f, x >= 0, x < precision f ]

-- | The "complement" of `rseq`, i.e., the series generated by `rseq`, with 0
-- replaced by 1, and 1 replaced by 0.
rseq' :: Series n -> Series n
rseq' f = Series $ V.map (1-) $ coeffVector (rseq f)

infixr 7 ?

-- | Select certain coefficients of the first series, based on indices from
-- the second series, returning the selection as a series.  Elements of the
-- second series that are not nonnegative integers or not less than the precision
-- are ignored; trailing zeros are also ignored.
(?) :: KnownNat n => Series n -> Series n -> Series n
(?) (Series v) c | c == 0 = series (Proxy :: Proxy n) [v ! 0]
(?) (Series v) c =
    series (Proxy :: Proxy n) $ map (v !) $
        dropTrailing 0 [ x | x <- intPrefix c, x >= 0, x < precision c ]

infixr 7 `o`

-- | The composition of two power series.
--
-- >>> let x = polynomial (Proxy :: Proxy 4) [0,1]
-- >>> (1/(1-x)) `o` (2*x)
-- series (Proxy :: Proxy 4) [Val (1 % 1),Val (2 % 1),Val (4 % 1),Val (8 % 1)]
--
o :: Series n -> Series n -> Series n
o (Series u) (Series v) = Series $ u `comp` v
{-# INLINE o #-}

c0 :: KnownNat n => (Rat -> Rat) -> Series n -> Series n
c0 f (Series v) = fromRat $ f (V.head v)
{-# INLINE c0 #-}

-- | Constant term of the given series.
constant :: Series n -> Rat
constant (Series v) = V.head v
{-# INLINE constant #-}

fromRat :: KnownNat n => Rat -> Series n
fromRat c = polynomial (Proxy :: Proxy n) [c]
{-# INLINE fromRat #-}

kRestrict :: Int -> Series n -> Series n
kRestrict k (Series v) = Series (v // [(k, Indet)])
{-# INLINE kRestrict #-}

restrict :: Series n -> Series n
restrict f = kRestrict (precision f - 1) f
{-# INLINE restrict #-}

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
{-# INLINE (!^!) #-}

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
      (n, k) ->
          case (d `mod` fromInteger k, k) of
            (0, 2) -> y * squareRoot (f/xpow d) ^^ n
            (0, _) -> y * exp' (fromRational r * log (f/xpow d))
            (_, _) -> polynomial (Proxy :: Proxy n) [Indet]
        where
          d = fromInteger $ leadingExponent f :: Int
          y = xpow ((d `div` fromInteger k) * fromInteger n)

leadingExponent :: Series n -> Integer
leadingExponent f =
    case span (==0) (rationalPrefix f) of
      (_ ,[]) -> 0
      (xs,_ ) -> fromIntegral (length xs)
{-# INLINE leadingExponent #-}

isConstant :: Series n -> Bool
isConstant (Series u) = V.all (==0) (V.tail u)
{-# INLINE isConstant #-}

squareRoot :: KnownNat n => Series n -> Series n
squareRoot f = squareRoot' (precision f - 1) f
  where
    squareRoot' 0 _ = 1
    squareRoot' d g =
        let h = 2 * squareRoot' (d-1) (kRestrict d g)
        in c0 sqrt g + integral (derivative g / h)

blackDiamond :: Series n -> Series n -> Series n
blackDiamond (Series u) (Series v) =
    Series $ V.generate (V.length u) $ \n ->
        sum [ u!(a+c) * v!(b+c) * multinomial [a,b,c]
            | [a,b,c] <- compositions 3 n
            ]

compositions :: Int -> Int -> [[Int]]
compositions 0 0 = [[]]
compositions 0 _ = []
compositions 1 n = [[n]]
compositions 2 n = [[n-i,i] | i <- [0..n]]
compositions k 0 = [ replicate k 0 ]
compositions k n = [0..n] >>= \i -> map (i:) (compositions (k-1) (n-i))

-- | The secant function: @sec f = 1 / cos f@
sec :: KnownNat n => Series n -> Series n
sec f = 1 / cos f
{-# INLINE sec #-}

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
