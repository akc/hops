module HOPS.GF.CFrac.Poly
    ( stieltjes
    , jacobi
    , jacobi0
    , jacobi1
    ) where

import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as IntMap

type Name = Int

type Env a = IntMap a

data Monomial a = Monomial a [(Name, Int)]
  deriving (Eq, Show)

newtype Poly a = Sum [Monomial a]
  deriving (Eq, Show)

simplifyMonomial :: Num a => Env a -> Monomial a -> Monomial a
simplifyMonomial env (Monomial c vars) = Monomial (c*val) vars'
  where
    (vals, vars') = partition (\(name, _) -> IntMap.member name env) vars
    val = foldr (\(name, i) c -> c*(env ! name)^i) 1 vals

simplify :: Num a => Env a -> Poly a -> Poly a
simplify env (Sum ms) = Sum $ joinEqual $ map (simplifyMonomial env) ms
  where
    joinEqual
        = map (\grp@(Monomial _ xs : _) -> Monomial (sum [ c | Monomial c _ <- grp ]) xs)
        . groupBy (\(Monomial _ xs) (Monomial _ ys) -> xs == ys)
        . sortOn (\(Monomial _ xs) -> xs)

solve :: (Eq a, Fractional a) => Poly a -> a -> Maybe (Name, a)
solve p c =
    case p of
      Sum [Monomial b [(name, 1)]]
          | b == 0    -> Nothing
          | otherwise -> Just (name, c/b)

      Sum [Monomial a [], Monomial b [(name, 1)]]
          | b == 0    -> Nothing
          | otherwise -> Just (name, (c-a)/b)

      _ -> Nothing

stieltjes :: (Eq a, Fractional a) => Vector a -> Vector a
stieltjes = V.fromList . stieltjes' . V.toList

stieltjes' :: (Eq a, Fractional a) => [a] -> [a]
stieltjes' [] = []
stieltjes' (0:_) = [0]
stieltjes' (x:xs) = x : map snd (IntMap.toAscList (fitS (map (/x) xs)))

jacobi' :: (Eq a, Fractional a) => [a] -> [a]
jacobi' [] = []
jacobi' (0:_) = [0]
jacobi' (x:xs) = x : map snd (IntMap.toAscList (fitJ (map (/x) xs)))

jacobi :: (Eq a, Fractional a) => Vector a -> (Vector a, Vector a)
jacobi xs = (V.fromList v, V.fromList u)
  where
    (u, v) = bisect $ jacobi' $ V.toList xs

jacobi0 :: (Eq a, Fractional a) => Vector a -> Vector a
jacobi0 = fst . jacobi

jacobi1 :: (Eq a, Fractional a) => Vector a -> Vector a
jacobi1 = snd . jacobi

bisect :: [a] -> ([a], [a])
bisect [] = ([], [])
bisect (x:xs) = let (ys, zs) = bisect xs in (x:zs, ys)

fitS :: (Eq a, Fractional a) => [a] -> Env a
fitS = fitS' IntMap.empty 1
  where
    fitS' env _ [] = env
    fitS' env k (x:xs) =
        case solve (simplify env (stieltjesPoly k)) x of
          Just (name, val) ->
              fitS' (IntMap.insert name val env) (k+1) xs
          Nothing ->
              env

fitJ :: (Eq a, Fractional a) => [a] -> Env a
fitJ = fitJ' IntMap.empty 1
  where
    fitJ' env _ [] = env
    fitJ' env k (x:xs) =
        case solve (simplify env (jacobiPoly k)) x of
          Just (name, val) ->
              fitJ' (IntMap.insert name val env) (k+1) xs
          Nothing ->
              env

-- Stieltjes-Rogers polynomials
stieltjesPoly :: (Eq a, Num a) => Int -> Poly a
stieltjesPoly n =
    Sum [ monomial (coeff ds) ds | ds <- compositions n ]
  where
    monomial c = Monomial c . zip [0..]
    coeff ds =
        product
           [ (d+d'-1) `choose` (d-1)
           | (d,d') <- zip ds (drop 1 ds)
           ]

-- Jacobi-Rogers polynomials
jacobiPoly :: (Eq a, Num a) => Int -> Poly a
jacobiPoly n =
    Sum [ monomial c t | t <- compositionPairs n, let c = coeff t, c /= 0 ]
  where
    coeff (ns, ms) = coeff1 ns ms * coeff2 ns ms
    coeff1 ns ms = let m = last ms in (m + last (1:ns) - 1) `choose` m
    coeff2 ns ms = product $ zipWith3 (\m n n' -> multinomial [m,n,n'-1]) ms ns (1:ns)
    monomial c (ns, ms) =
        Monomial c
          $ [ (2*i+1, n) | (i, n) <- zip [0..] ns, n /= 0 ] ++
            [ (2*i,   m) | (i, m) <- zip [0..] ms, m /= 0 ]

compositionPairs :: Int -> [ ([Int], [Int]) ]
compositionPairs p = [0 .. p] >>= \k -> kCompositionPairs k p

kCompositionPairs :: Int -> Int -> [ ([Int], [Int]) ]
kCompositionPairs k p =
    [0 .. p `div` 2] >>= \n ->
        (,) <$> kCompositions k n <*> kCompositions0 (k+1) (p-2*n)

compositions :: Int -> [[Int]]
compositions 0 = [[]]
compositions n = [ c:cs | c <- [1..n], cs <- compositionStream !! (n-c) ]

compositionStream :: [[[Int]]]
compositionStream = map compositions [0..]

-- Weak k-compositions
kCompositions0 :: Int -> Int -> [[Int]]
kCompositions0 0 0 = [[]]
kCompositions0 0 _ = []
kCompositions0 k n = [0..n] >>= \i -> map (i:) (kCompositions0 (k-1) (n-i))

kCompositions :: Int -> Int -> [[Int]]
kCompositions 0 0 = [[]]
kCompositions 0 _ = []
kCompositions k n = [1..n] >>= \i -> map (i:) (kCompositions (k-1) (n-i))

choose :: Num a => Int -> Int -> a
choose n k
    | n == k = 1
    | n < 0 || k < 0 || n < k = 0
    | otherwise = fromInteger $ pascal !! n !! k

pascal :: [[Integer]]
pascal = iterate (\row -> zipWith (+) (0 : row) (row ++ [0])) [1]

multinomial :: Num a => [Int] -> a
multinomial [] = 1
multinomial [_] = 1
multinomial [k0,k1] = (k0+k1) `choose` k0
multinomial [k0,k1,k2] = (k0+k1+k2) `choose` k0 * (k1+k2) `choose` k1
multinomial ks@(k:kt) = fromIntegral (sum ks) `choose` k * multinomial kt
