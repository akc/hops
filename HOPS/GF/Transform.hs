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
data Transform n = Transform Int ([Series n] -> Series n)

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

shift :: Series n -> Series n
shift f@(Series u)
    | V.null u  = f
    | otherwise = Series $ V.snoc (V.tail u) Indet

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
    lift (restrictToPrefix (length cs)) $ shift (1 / product gs)
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
    lift (restrictToPrefix (length cs)) $ shift (product gs)
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
  map (Transform 1 <$>)
    [ ("absolute",   \[Series v] -> Series (V.map abs v))
    , ("bisect0",    \[f] -> lift bisect0 f)
    , ("bisect1",    \[f] -> lift bisect1 f)
    , ("bousi",      \[f] -> bousi f)
    , ("bous",       \[f] -> bous f)
    , ("cyc",        \[f] -> ccycle f)
    , ("dirichleti", \[f] -> lift dirichleti f)
    , ("euleri",     \[f] -> euleri f)
    , ("euler",      \[f] -> euler f)
    , ("hankel",     \[f] -> lift hankel f)
    , ("indicator",  \[f] -> rseq f)
    , ("indicatorc", \[f] -> rseq' f)
    , ("shift",      \[f] -> shift f)
    , ("mobiusi",    \[f] -> liftAny dirichlet [1/(1-x), f])
    , ("mobius",     \[f] -> liftAny dirichlet [lift dirichleti (1/(1-x)), f])
    , ("mset",       \[f] -> multiset f)
    , ("partition",  \[f] -> multiset $ rseq f)
    , ("point",      \[f] -> x*derivative f)
    , ("prods",      \[f] -> lift (V.drop 1 . V.scanl (*) 1) f)
    , ("pset",       \[f] -> powerset f)
    , ("seq",        \[f] -> 1/(1 - f))
    , ("T019",       \[f] -> t019 f)
    , ("trisect0",   \[f] -> lift trisect0 f)
    , ("trisect1",   \[f] -> lift trisect1 f)
    , ("trisect2",   \[f] -> lift trisect2 f)
    , ("weight",     \[f] -> weight f)
    , ("laplacei",   \[f] -> laplacei f)
    , ("laplace",    \[f] -> laplace f)
    , ("revert",     \[f] -> revert f)
    , ("diff",       \[f] -> derivative f)
    , ("int",        \[f] -> integral f)
    , ("delta",      \[f] -> lift finDiff f)
    , ("sqrt",       \[f] -> sqrt f)
    , ("abs",        \[f] -> abs f)
    , ("sgn",        \[f] -> x/abs f)
    , ("log",        \[f] -> log f)
    , ("exp",        \[f] -> exp f)
    , ("arsinh",     \[f] -> asinh f)
    , ("arcosh",     \[f] -> acosh f)
    , ("artanh",     \[f] -> atanh f)
    , ("arcsin",     \[f] -> asin f)
    , ("arccos",     \[f] -> acos f)
    , ("arctan",     \[f] -> atan f)
    , ("sinh",       \[f] -> sinh f)
    , ("cosh",       \[f] -> cosh f)
    , ("tanh",       \[f] -> tanh f)
    , ("sin",        \[f] -> sin f)
    , ("cos",        \[f] -> cos f)
    , ("tan",        \[f] -> tan f)
    , ("sec",        \[f] -> sec f)
    , ("neg",        \[f] -> negate f)
    , ("fac",        \[f] -> fac f)
    ]
  ++ map (Transform 2 <$>)
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
    , ("dirichlet",  liftAny dirichlet)
    ]

dispatch :: KnownNat n => Map ByteString (Transform n)
dispatch = M.fromList associations

-- | Lookup a transform by name.
lookupTransform :: KnownNat n => ByteString -> Maybe (Transform n)
lookupTransform name = M.lookup name dispatch

-- | The list of all names of transforms.
transforms :: [ByteString]
transforms = map fst (associations :: [(ByteString, Transform 0)])
