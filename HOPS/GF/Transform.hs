{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module HOPS.GF.Transform
    ( Transform(..)
    , lookupTransform
    , transforms
    ) where

import GHC.TypeLits
import Data.Proxy
import Data.Ratio
import Data.Maybe
import Data.List (foldl')
import Data.Vector (Vector, (!), (!?), (//))
import qualified Data.Vector as V
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.ByteString.Char8 (ByteString)
import HOPS.GF.Series
import HOPS.Utils.Matrix

-- | A `Transform` takes zero or more `Series` to another `Series`.
data Transform n = Transform1 (Series n -> Series n)
                 | TransformAny ([Series n] -> Series n)
                 | TransformK Int ([Series n] -> Series n)

x :: KnownNat n => Series n
x = xpow 1

facSeries :: KnownNat n => Series n
facSeries = series (Proxy :: Proxy n) $ scanl (*) 1 (map fromIntegral [1::Int ..])

lift :: (Vector Rat -> Vector Rat) -> Series n -> Series n
lift f (Series cs) = Series (f cs)

liftAny :: ([Vector Rat] -> Vector Rat) -> [Series n] -> Series n
liftAny f ss = Series (f [ cs | (Series cs) <- ss ])

uncons :: Vector a -> Maybe (a, Vector a)
uncons u
    | V.null u  = Nothing
    | otherwise = Just (V.head u, V.tail u)

bisect0 :: Vector Rat -> Vector Rat
bisect0 u =
    case uncons u of
      Nothing      -> V.empty
      Just (c, u') -> V.cons c (bisect1 u')

bisect1 :: Vector Rat -> Vector Rat
bisect1 u =
    case uncons u of
      Nothing      -> V.empty
      Just (_, u') -> V.snoc (bisect0 u') Indet

trisect0 :: Vector Rat -> Vector Rat
trisect0 u =
    case uncons u of
      Nothing      -> V.empty
      Just (c, u') -> V.cons c (trisect2 u')

trisect1 :: Vector Rat -> Vector Rat
trisect1 u =
    case uncons u of
      Nothing      -> V.empty
      Just (_, u') -> V.snoc (trisect0 u') Indet

trisect2 :: Vector Rat -> Vector Rat
trisect2 u =
    case uncons u of
      Nothing      -> V.empty
      Just (_, u') -> V.snoc (trisect1 u') Indet

shiftLeft :: Series n -> Series n
shiftLeft f@(Series u)
    | V.null u  = f
    | otherwise = Series $ V.snoc (V.tail u) Indet

shiftRight :: Series n -> Series n
shiftRight f@(Series u)
    | V.null u  = f
    | otherwise = Series $ V.cons 1 (V.init u)

finDiff :: Vector Rat -> Vector Rat
finDiff u =
    case uncons u of
      Nothing      -> V.empty
      Just (_, u') -> V.snoc (V.zipWith (-) u' u) Indet

-- The Mobius function of the poset of integers under divisibility
mobiusFun :: Integer -> Integer -> Integer
mobiusFun a b
  | a == b         = 1
  | b `rem` a == 0 = -sum [ mobiusFun a c | c <- [a..b-1], b `rem` c == 0 ]
  | otherwise      = 0

-- The number theoretical Mobius function
mu :: Integer -> Integer
mu = mobiusFun 1

-- The number theoretical Dirichlet convolution
dirichlet :: [Vector Rat] -> Vector Rat
dirichlet [f,g] = V.generate (max (V.length f) (V.length g)) (\i -> term (i+1))
  where
    term n = sum [ fromMaybe 0 ((*) <$> f !? (n `div` d - 1) <*> g !? (d - 1))
                 | d <- [1..n]
                 , n `mod` d == 0
                 ]
dirichlet _ = V.empty

dirichleti :: Vector Rat -> Vector Rat
dirichleti f = foldl' upd (V.replicate (length f) 0) [1..length f]
  where
    upd g 1 = g // [(0, 1 / V.head f)]
    upd g n = let s = sum [ (f ! (n `div` d - 1)) * (g ! (d - 1))
                          | d <- [1..n-1]
                          , n `mod` d == 0
                          ]
              in g // [(n-1, -s / V.head f)]


restrictToPrefix :: Int -> Vector Rat -> Vector Rat
restrictToPrefix n = V.imap $ \i c -> if i < n then c else Indet

euler :: KnownNat n => Series n -> Series n
euler f =
    lift (restrictToPrefix (length cs)) $ shiftLeft (1 / product gs)
  where
    cs = map Val (rationalPrefix f)
    gs = zipWith (\k c -> (1 - xpow k) ^! c) [1::Int ..] cs

euleri :: Series n -> Series n
euleri (Series u) = Series $ V.generate (V.length u) (\i -> term (i+1))
  where
    f 1 = u ! 0
    f n = fromIntegral n * u ! (n-1) - sum [ f k * u ! (n-k-1) | k <- [1..n-1] ]
    term n = Val (1 % fromIntegral n) *
                 sum [ Val m * f d
                     | d <- [1..n]
                     , let m = fromIntegral (mu (fromIntegral (n `div` d)))
                     , n `rem` d == 0
                     ]

weight :: KnownNat n => Series n -> Series n
weight f =
    lift (restrictToPrefix (length cs)) $ shiftLeft (product gs)
  where
    cs = map Val (rationalPrefix f)
    gs = zipWith (\n c -> (1 + xpow n) ^! c) [1::Int ..] cs

mask :: KnownNat n => Series n -> Series n
mask = series (Proxy :: Proxy n) . tail . scanl h 0 . coeffList
  where
    h DZ _      = DZ
    h v (Val _) = v
    h _ Indet   = Indet
    h _ DZ      = DZ

multiset :: KnownNat n => Series n -> Series n
multiset f
    | constant f /= 0 = infty
    | otherwise       = mask f + 1 / product gs
  where
    cs = rationalPrefix f
    gs = zipWith (\k c -> (1 - xpow k) ^! Val c) [1::Int ..] (tail cs)

powerset :: KnownNat n => Series n -> Series n
powerset f
    | constant f /= 0 = infty
    | otherwise       = mask f + product gs
  where
    cs = rationalPrefix f
    gs = zipWith (\k c -> (1 + xpow k) ^! Val c) [1::Int ..] (tail cs)

ccycle :: KnownNat n => Series n -> Series n
ccycle f = sum $ map g [1::Int .. precision f - 1]
  where
    g k = fromRational (fromIntegral (totient k) % fromIntegral k) *
            log ( 1/(1-f `o` xpow k))

totient :: Int -> Int
totient 1 = 1
totient a = length $ filter (\k -> gcd a k == 1) [1..a-1]

-- T019: a[j+2]-2*a[j+1]+a[j] where a[j] = j-th term of the sequence
t019 :: KnownNat n => Series n -> Series n
t019 f = ((1-x)*(1-x)*f + (2*a0 - a1)*x - a0) / (x*x)
  where
    v  = coeffVector f
    a0 = polynomial (Proxy :: Proxy n) [v!0]
    a1 = polynomial (Proxy :: Proxy n) [v!1]

-- The Boustrophedon transform
bous :: KnownNat n => Series n -> Series n
bous f = laplace (laplacei f * (sec x + tan x))

-- The inverse Boustrophedon transform
bousi :: KnownNat n => Series n -> Series n
bousi f = laplace (laplacei f / (sec x + tan x))

hankel :: Vector Rat -> Vector Rat
hankel v = V.generate n $ \i -> det (hankelN (i+1))
  where
    n = V.length v
    hankelN i = V.take i $ V.map (V.take i) hankelMatrix
    hankelMatrix = V.iterateN n (\u -> V.snoc (V.tail u) Indet) v

laplace :: KnownNat n => Series n -> Series n
laplace f = f .* facSeries

laplacei :: KnownNat n => Series n -> Series n
laplacei f = f ./ facSeries

associations :: KnownNat n => [(ByteString, Transform n)]
associations =
  map (Transform1 <$>)
    [ ("ABS",        \(Series v) -> Series (V.map abs v))
    , ("BARRY1",     \f -> 1 / (1 - x - x*x*f)) -- Named after Paul Barry
    , ("BARRY2",     \f -> 1 / (1 + x + x*x*f)) -- Named after Paul Barry
    , ("BINOMIALi",  \f -> laplace(exp (-x) * laplacei f))
    , ("BINOMIAL",   \f -> laplace(exp x * laplacei f))
    , ("BIN1",       \f -> shiftLeft $ laplace(-exp (-x) * (laplacei (x*f) `o` (-x))))
    , ("BISECT0",    lift bisect0)
    , ("BISECT1",    lift bisect1)
    , ("BOUSi",      bousi)
    , ("BOUS",       bous)
    , ("CATALANi",   \f -> f `o` (x*(1-x)))
    , ("CATALAN",    \f -> let cat = 2/(1+sqrt(1-4*x)) in f `o` (x*cat))
    , ("CYC",        ccycle)
    , ("DIFF",       lift finDiff)
    , ("D",          derivative)
    , ("DIRICHLETi", lift dirichleti)
    , ("EULERi",     euleri)
    , ("EULER",      euler)
    , ("EXP",        \f -> shiftLeft $ laplace (exp (laplacei (x*f))))
    , ("lHANKEL",    \f -> let g = f .* f - shiftLeft f .* shiftRight f in shiftLeft g)
    , ("HANKEL",     lift hankel)
    , ("I",          rseq)
    , ("IC",         rseq')
    , ("LAHi",       \f -> laplace(laplacei f `o` (x/(1+x))))
    , ("LAH",        \f -> laplace(laplacei f `o` (x/(1-x))))
    , ("LEFT",       shiftLeft)
    , ("LOG",        \f -> shiftLeft $ laplace(log (1 + laplacei (x*f))))
    , ("MOBIUSi",    \f -> liftAny dirichlet [1/(1-x), f])
    , ("MOBIUS",     \f -> liftAny dirichlet [lift dirichleti (1/(1-x)), f])
    , ("MSET",       multiset)
    , ("PARTITION",  multiset . rseq)
    , ("POINT",      \f -> x*derivative f)
    , ("PRODS",      lift $ V.drop 1 . V.scanl (*) 1)
    , ("PSET",       powerset)
    , ("RIGHT",      shiftRight)
    , ("SEQ",        \f -> 1/(1 - f))
    , ("STIRLINGi",  \f -> shiftLeft $ laplace $ laplacei (x*f) `o` log (x+1))
    , ("STIRLING",   \f -> shiftLeft $ laplace $ laplacei (x*f) `o` (exp x - 1))
    , ("T019",       t019)
    , ("TRISECT0",   lift trisect0)
    , ("TRISECT1",   lift trisect1)
    , ("TRISECT2",   lift trisect2)
    , ("WEIGHT",     weight)
    , ("laplacei",   laplacei)
    , ("laplace",    laplace)
    , ("revert",     revert)
    , ("integral",   integral)
    , ("sqrt",       sqrt)
    , ("abs",        abs)
    , ("log",        log)
    , ("exp",        exp)
    , ("arsinh",     asinh)
    , ("arcosh",     acosh)
    , ("artanh",     atanh)
    , ("arcsin",     asin)
    , ("arccos",     acos)
    , ("arctan",     atan)
    , ("sinh",       sinh)
    , ("cosh",       cosh)
    , ("tanh",       tanh)
    , ("sin",        sin)
    , ("cos",        cos)
    , ("tan",        tan)
    , ("sec",        sec)
    , ("neg",        negate)
    , ("fac",        fac)
    ]
  ++ map (TransformK 2 <$>)
    [ ("add",        \[f,g] -> (+) f g)
    , ("sub",        \[f,g] -> (-) f g)
    , ("mul",        \[f,g] -> (*) f g)
    , ("div",        \[f,g] -> (/) f g)
    , ("bdp",        \[f,g] -> blackDiamond f g)
    , ("pow",        \[f,g] -> (**) f g)
    , ("comp",       \[f,g] -> o f g)
    , ("coef",       \[f,g] -> (?) f g)
    , ("ptmul",      \[f,g] -> (.*) f g)
    , ("ptdiv",      \[f,g] -> (./) f g)
    , ("DIRICHLET",  liftAny dirichlet)
    ]

dispatch :: KnownNat n => Map ByteString (Transform n)
dispatch = M.fromList associations

-- | Lookup a transform by name.
lookupTransform :: KnownNat n => ByteString -> Maybe (Transform n)
lookupTransform name = M.lookup name dispatch

-- | The list of all names of transforms.
transforms :: [ByteString]
transforms = map fst (associations :: [(ByteString, Transform 0)])
