-- |
-- Copyright   : Anders Claesson 2017
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module HOPS.GF.CFrac.Reference
    ( stieltjes
    , jacobi
    , jacobi0
    , jacobi1
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V

type Name = String

data Series a = Series [a] deriving Show

data Expr a
    = Lit a
    | Var Name
    | Add (Expr a) (Expr a)
    | Mul (Expr a) (Expr a)
  deriving (Eq, Show)

instance (Eq a, Num a) => Num (Expr a) where
    Lit 0 + e = e
    e + Lit 0 = e
    e1 + e2 = Add e1 e2
    Lit 1 * e = e
    e * Lit 1 = e
    e1 * e2 = Mul e1 e2
    fromInteger = Lit . fromInteger
    abs = undefined
    signum = undefined
    negate = undefined

instance Num a => Num (Series a) where

    Series xs + Series ys =
        Series $ zipWith (+) xs ys

    Series xs - Series ys =
        Series $ zipWith (-) xs ys

    Series (x:xs) * g@(Series (y:ys)) =
        let
            Series zs = Series xs * g + cmult x (Series ys)
        in
            Series $ x*y : zs

    abs = undefined
    signum = undefined
    fromInteger x = Series $ fromInteger x : repeat 0

cadd :: Num a => a -> Series a -> Series a
cadd c (Series (x:xs)) = Series (c + x : xs)

cmult :: Num a => a -> Series a -> Series a
cmult c (Series xs) = Series $ map (c*) xs

-- 1/(1-xf)
geometric :: Num a => Series a -> Series a
geometric f = g
  where
    g = Series (1 : ds)
    Series ds = f * g

subs :: (Name, a) -> Expr a -> Expr a
subs t@(name, val) expr =
    case expr of
      Var name' | name == name' ->
          Lit val
      Add e1 e2 ->
          Add (subs t e1) (subs t e2)
      Mul e1 e2 ->
          Mul (subs t e1) (subs t e2)
      _ ->
          expr

locus :: (a -> a -> Bool) -> [a] -> a
locus _ [] = undefined
locus _ [_] = undefined
locus p (x:ys@(y:_))
    | p x y     = y
    | otherwise = locus p ys

simplify :: (Eq a, Num a) => Expr a -> Expr a
simplify = locus (==) . iterate f
 where
   f (Add (Lit 0) e) = f e
   f (Add (Lit x) (Lit y)) = Lit (x+y)
   f (Add (Lit x) (Add (Lit y) e)) = Add (Lit (x+y)) $ f e
   f (Add (Lit x) (Add e (Lit y))) = Add (Lit (x+y)) $ f e
   f (Add e (Lit x)) = Add (Lit x) (f e)
   f (Add e1 e2) = Add (f e1) (f e2)
   f (Mul (Lit 0) _) = Lit 0
   f (Mul (Lit 1) e) = f e
   f (Mul (Lit x) (Lit y)) = Lit (x*y)
   f (Mul (Lit x) (Add (Lit y) e)) = Add (Lit (x*y)) (Mul (Lit x) (f e))
   f (Mul (Lit x) (Add e (Lit y))) = Add (Lit (x*y)) (Mul (Lit x) (f e))
   f (Mul (Lit x) (Mul (Lit y) e)) = Mul (Lit (x*y)) (f e)
   f (Mul e (Lit x)) = Mul (Lit x) (f e)
   f (Mul e1 e2) = Mul (f e1) (f e2)
   f e = e

solve1 :: (Eq a, Fractional a) => Expr a -> a -> Maybe (Name, a)
solve1 e c =
    case simplify e of
      Var x -> Just (x, c)
      Mul (Lit b) (Var x) -> Just (x, c/b)
      Add (Lit a) (Var x) -> Just (x, c-a)
      Add (Lit a) (Mul (Lit b) (Var x)) -> Just (x, (c-a)/b)
      _ -> Nothing

solve :: (Eq a, Fractional a) => [Expr a] -> [a] -> [(Name, a)]
solve [] _  = []
solve _  [] = []
solve (e:es) (x:xs) =
    case solve1 e x of
      Just solution ->
          solution : solve (map (subs solution) es) xs
      Nothing ->
          []

sfrac :: (Eq a, Num a) => Int -> Int -> Series (Expr a)
sfrac n k
    | k == n = 1
    | otherwise =
        let
            a_k = Var ("a" ++ show k)
            f = cmult a_k (sfrac n (k+1))
        in
            geometric f

jfrac :: (Eq a, Num a) => Int -> Int -> Series (Expr a)
jfrac n k
    | k == n = 1
    | otherwise =
        let
            a_k = Var ("a" ++ show k)
            b_k = Var ("b" ++ show k)
            Series cs = cmult b_k (jfrac n (k+1))
            f = cadd a_k (Series (0 : cs))
        in
            geometric f

stieltjes' :: (Eq t, Fractional t) => [t] -> [t]
stieltjes' [] = []
stieltjes' (0:_) = [0]
stieltjes' (x:xs) = x : map snd (solve es (map (/x) xs))
  where
    Series (1:es) = sfrac n 0
    n = length xs

stieltjes :: (Eq t, Fractional t) => Vector t -> Vector t
stieltjes = V.fromList . stieltjes' . V.toList

jacobi' :: (Eq t, Fractional t) => [t] -> [t]
jacobi' [] = []
jacobi' (0:_) = [0]
jacobi' (x:xs) = x : map snd (solve es (map (/x) xs))
  where
    Series (1:es) = jfrac n 0
    n = length xs

jacobi :: (Eq t, Fractional t) => Vector t -> (Vector t, Vector t)
jacobi w = (V.fromList v, V.fromList u)
  where
    (u, v) = bisect $ jacobi' $ V.toList w

jacobi0 :: (Eq a, Fractional a) => Vector a -> Vector a
jacobi0 = fst . jacobi

jacobi1 :: (Eq a, Fractional a) => Vector a -> Vector a
jacobi1 = snd . jacobi

bisect :: [a] -> ([a], [a])
bisect [] = ([], [])
bisect (x:xs) = let (ys, zs) = bisect xs in (x:zs, ys)
