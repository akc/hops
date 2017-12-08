{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Copyright   : Anders Claesson 2015-2017
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

import GHC.TypeLits
import Text.Printf
import Data.List
import Data.Maybe
import Data.Semigroup
import Data.Ratio
import Data.Proxy
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Control.Applicative
import Control.Monad
import System.Exit
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck
import Test.QuickCheck.Test
import Test.QuickCheck.Modifiers
import HOPS.OEIS
import HOPS.Utils.Matrix
import HOPS.GF
import qualified HOPS.GF.Const as C
import qualified HOPS.GF.Rats as R
import HOPS.DB
import HOPS.Config
import qualified HOPS.GF.CFrac.Hankel as CFrac.Hankel
import qualified HOPS.GF.CFrac.Poly as CFrac.Poly
import qualified HOPS.GF.CFrac.QD as CFrac.QD
import qualified HOPS.GF.CFrac.Reference as CFrac.Reference

type S5  = Series 5
type S10 = Series 10
type S20 = Series 20

-- Series all of whose coefficients (with index < n) are nonzero.
newtype CoeffwiseNonzero n = CoeffwiseNonzero (Series n) deriving Show

-- Series of the form `x*f` (i.e. the constant term is 0).
newtype Revertible n = Revertible (Series n) deriving Show

-- Series with nonzero constant term (a unit in Q[[x]]).
newtype Unit n = Unit (Series n) deriving Show

-- Series with at least one nonzero rational coeff.
newtype NonNil n = NonNil (Series n) deriving Show

-- Series with no DZ coeff.
newtype NonDZ n = NonDZ (Series n) deriving Show

-- Series whose terms are all rationals (i.e. not Indet or DZ).
newtype Full n = Full (Series n) deriving Show

-- Full series with at least one nonzero rational coeff.
newtype Full1 n = Full1 (Series n) deriving Show

-- Unit series that is also Full.
newtype FullUnit n = FullUnit (Series n) deriving Show

-- Revertible NonNil series
newtype Revertible1 n = Revertible1 (Series n) deriving Show

-- Full Revertible NonNil series.
newtype FullRevertible1 n = FullRevertible1 (Series n) deriving Show

-- Full invertible series (has non-zero unit coefficient).
newtype FullInvertible n = FullInvertible (Series n) deriving Show

alpha :: String
alpha = ['A'..'Z'] ++ ['a'..'z']

digits :: String
digits = ['0'..'9']

ratsGen :: Gen R.Rats
ratsGen = (,,) <$> resize 4 arbitrary <*> arbitrary <*> arbitrary

nameGen :: Gen Name
nameGen = B.pack <$> ((:) <$> first <*> rest)
  where
    first = elements alpha
    rest = listOf $ elements (alpha ++ digits ++ "_")

smallRationalGen :: Gen Rational
smallRationalGen = (\x (Positive y) -> x%y) <$> arbitrary <*> arbitrary

valGen :: Gen Rat
valGen = Val <$> frequency [(35, smallRationalGen), (5, arbitrary)]

nonzeroValGen :: Gen Rat
nonzeroValGen = valGen `suchThat` (/=0)

valGenStream :: Gen [Rat]
valGenStream = infiniteListOf valGen

instance Arbitrary Rat where
    arbitrary = frequency
        [ ( 4, return Indet )
        , ( 1, return DZ )
        , (55, fromInteger <$> arbitrary)
        , (40, valGen)
        ]

instance KnownNat n => Arbitrary (Series n) where
    arbitrary = series (Proxy :: Proxy n) <$> arbitrary

instance KnownNat n => Arbitrary (CoeffwiseNonzero n) where
    arbitrary = CoeffwiseNonzero <$> arbitrary `suchThat` coeffWiseNonzero

instance KnownNat n => Arbitrary (Revertible n) where
    arbitrary = Revertible . series (Proxy :: Proxy n) . (0:) <$> arbitrary

instance KnownNat n => Arbitrary (Unit n) where
    arbitrary = Unit . series (Proxy :: Proxy n) <$> ((:) <$> nonzeroValGen <*> arbitrary)

nonzeroVal :: Rat -> Bool
nonzeroVal (Val r) | r /= 0 = True
nonzeroVal _ = False

coeffWiseNonzero :: Series n -> Bool
coeffWiseNonzero = all nonzeroVal . coeffList

nonzeroLeadingVal :: Series n -> Bool
nonzeroLeadingVal = nonzeroVal . leadingCoeff

nonzeroConstant :: Series n -> Bool
nonzeroConstant = nonzeroVal . constant

instance KnownNat n => Arbitrary (NonNil n) where
    arbitrary = NonNil <$> arbitrary `suchThat` nonzeroLeadingVal

instance KnownNat n => Arbitrary (NonDZ n) where
    arbitrary = NonDZ . series (Proxy :: Proxy n) <$> arbitrary `suchThat` notElem DZ

instance KnownNat n => Arbitrary (Full n) where
    arbitrary = Full . series (Proxy :: Proxy n) <$> valGenStream

instance KnownNat n => Arbitrary (Full1 n) where
    arbitrary = Full1
      <$> (series (Proxy :: Proxy n) <$> valGenStream) `suchThat` nonzeroLeadingVal

instance KnownNat n => Arbitrary (FullUnit n) where
    arbitrary = FullUnit . series (Proxy :: Proxy n)
      <$> ((:) <$> nonzeroValGen <*> valGenStream)

instance KnownNat n => Arbitrary (Revertible1 n) where
    arbitrary = Revertible1
      <$> (series (Proxy :: Proxy n) . (0:) <$> arbitrary) `suchThat` nonzeroLeadingVal

instance KnownNat n => Arbitrary (FullRevertible1 n) where
    arbitrary = FullRevertible1
      <$> (series (Proxy :: Proxy n) . (0:) <$> valGenStream) `suchThat` nonzeroLeadingVal

instance KnownNat n => Arbitrary (FullInvertible n) where
    arbitrary = FullInvertible
      <$> (series (Proxy :: Proxy n) <$> valGenStream) `suchThat` nonzeroConstant

instance Arbitrary Expr where
    arbitrary = frequency
        [ (30, Singleton <$> arbitrary)
        , (60, ELet <$> nameGen <*> arbitrary)
        , (10, ESeq <$> arbitrary <*> arbitrary)
        ]

instance Arbitrary Expr0 where
    arbitrary = frequency
        [ ( 3, EAdd   <$> arbitrary <*> arbitrary)
        , ( 3, ESub   <$> arbitrary <*> arbitrary)
        , (94, Expr1  <$> arbitrary)
        ]

instance Arbitrary Expr1 where
    arbitrary = frequency
        [ ( 2, EMul   <$> arbitrary <*> arbitrary)
        , ( 2, EDiv   <$> arbitrary <*> arbitrary)
        , ( 2, EPtMul <$> arbitrary <*> arbitrary)
        , ( 2, EPtDiv <$> arbitrary <*> arbitrary)
        , (92, Expr2  <$> arbitrary)
        ]

instance Arbitrary Expr2 where
    arbitrary = frequency
        [ ( 1, ENeg   <$> arbitrary)
        , ( 1, EPos   <$> arbitrary)
        , ( 1, EFac   <$> arbitrary)
        , ( 1, EPow   <$> arbitrary <*> arbitrary)
        , ( 1, EComp  <$> arbitrary <*> arbitrary)
        , (95, Expr3  <$> arbitrary)
        ]

instance Arbitrary Expr3 where
    arbitrary = frequency
        [ (20, return EX)
        , (15, EA     <$> arbitrary)
        , ( 3, ETag   <$> arbitrary)
        , (20, EVar   <$> nameGen  )
        , (33, ELit   <$> arbitrary)
        , ( 1, EApp   <$> nameGen <*> arbitrary)
        , ( 7, ERats  <$> ratsGen  )
        , ( 1, Expr   <$> arbitrary)
        ]

instance Arbitrary C.Expr0 where
    arbitrary = frequency
        [ ( 1, C.EAdd  <$> arbitrary <*> arbitrary)
        , ( 1, C.ESub  <$> arbitrary <*> arbitrary)
        , (98, C.Expr1 <$> arbitrary)
        ]

instance Arbitrary C.Expr1 where
    arbitrary = frequency
        [ ( 1, C.EMul  <$> arbitrary <*> arbitrary)
        , ( 1, C.EDiv  <$> arbitrary <*> arbitrary)
        , (98, C.Expr2 <$> arbitrary)
        ]

instance Arbitrary C.Expr2 where
    arbitrary = frequency
        [ ( 1, C.ENeg  <$> arbitrary)
        , ( 1, C.EPos  <$> arbitrary)
        , ( 1, C.EFac  <$> arbitrary)
        , ( 1, C.EPow  <$> arbitrary <*> arbitrary)
        , (96, C.Expr3 <$> arbitrary)
        ]

instance Arbitrary C.Expr3 where
    arbitrary = frequency
        [ (90, C.ELit  <$> arbitrary)
        , ( 9, return C.EN)
        , ( 1, C.Expr0 <$> arbitrary)
        ]

instance Arbitrary R.Term where
    arbitrary = frequency
        [ (10, return R.Ellipsis)
        , (45, R.Constant <$> arbitrary)
        , (45, R.Fun <$> arbitrary)
        ]

instance Arbitrary R.SequenceType where
  arbitrary = elements [R.Poly, R.Ser]

instance Arbitrary a => Arbitrary (Matrix a) where
    arbitrary = do
      n <- arbitrary :: Gen Int
      xss <- replicateM n (vector n)
      return $ matrix xss

contactOfOrder :: Int -> Series n -> Series n -> Bool
contactOfOrder m f g =
    let m' = if m < 0 then precision f + m else m
    in take m' (rationalPrefix f) == take m' (rationalPrefix g)

infix 4 ~=

(~=) :: Series n -> Series n -> Bool
f ~= g = rationalPrefix f == rationalPrefix g

infix 4 ~<

(~<) :: Series n -> Series n -> Bool
f ~< g = rationalPrefix f `isPrefixOf` rationalPrefix g

infix 4 ~==

f ~== g = as `isPrefixOf` bs || bs `isPrefixOf` as
  where
    as = rationalPrefix f
    bs = rationalPrefix g

check :: Testable prop => Int -> prop -> IO Result
check n = quickCheckWithResult stdArgs {maxSuccess = n}

evalExpr :: KnownNat n => Env n -> Expr -> Series n
evalExpr env = evalCore env . core

evalExpr1 :: KnownNat n => Expr -> Series n
evalExpr1 = evalCore emptyEnv . core

runExpr :: KnownNat n => Env n -> ByteString -> Series n
runExpr env = evalExpr env . fromMaybe (error "parse error") . parseExpr

runExpr1 :: KnownNat n => ByteString -> Series n
runExpr1 = runExpr emptyEnv

ogf :: KnownNat n => Proxy n -> [Integer] -> Series n
ogf n = series n . map (Val . fromIntegral)

ogf10 = ogf (Proxy :: Proxy 10)
ogf20 = ogf (Proxy :: Proxy 20)

stubDB :: KnownNat n => Vector (Series n)
stubDB = unsafePerformIO $ readANumDB =<< getConfig
{-# NOINLINE stubDB #-}

splitEqn :: ByteString -> (ByteString, ByteString)
splitEqn bs = (lhs', rhs')
  where
    (lhs, _:rhs) = break B.null (B.split '=' bs)
    lhs' = B.intercalate "=" lhs
    rhs' = B.intercalate "=" rhs

evalEqn :: KnownNat n => [Series n] -> ByteString -> (Series n, Series n)
evalEqn fs eqn = (exec lhs, exec rhs)
  where
    (lhs, rhs) = splitEqn eqn
    exec = runExpr $ Env stubDB (M.fromList (zip nameSupply fs))

prop :: KnownNat n => [Series n] -> ByteString -> Bool
prop fs = uncurry (==) . evalEqn fs

propN :: KnownNat n => Int -> [Series n] -> ByteString -> Bool
propN m fs = uncurry (contactOfOrder m) . evalEqn fs

areSimEq :: KnownNat n => [Series n] -> ByteString -> Bool
areSimEq fs = uncurry (~=) . evalEqn fs

prop' :: KnownNat n => [Series n] -> ByteString -> Bool
prop' = propN (-1)

prop1 :: ByteString -> Bool
prop1 = prop [nil::S20]

prop1' :: ByteString -> Bool
prop1' = prop' [nil::S20]

propN1 :: Int -> ByteString -> Bool
propN1 m = propN m [nil::S20]

propN1' :: ByteString -> Bool
propN1' = prop' [nil::S20]

prop_Expr_assoc p q r = pretty (p <> (q <> r)) == pretty ((p <> q) <> (r :: Expr))

prop_Expr_value p = evalExpr1 p == (evalExpr1 q :: Series 40)
  where
    q = p <> fromJust (parseExpr "stdin") :: Expr

prop_Rat_power_u = (1/4) !^! (3/2) == Val (1 % 8)
prop_Neg_power_u = prop1 "{-(-1)^n} == -1/(1+x)"

prop_shift_u      = prop1 "shift({4,3,2,1}) == {3,2,1}"
prop_bisect0_u    = prop1 "bisect0({0,1,2,3,4,5}) == {0,2,4}"
prop_bisect1_u    = prop1 "bisect1({0,1,2,3,4,5}) == {1,3,5}"
prop_bous_u       = prop1 "bous({5,4,3,2,1}) == {5,9,16,33,84}"
prop_bousi_u      = prop1 "bousi({5,4,3,2,1}) == {5,-1,0,-5,4}"
prop_cyc_u        = prop1 "cyc({0,1,1,1,1,1}) == {0,1,2,3,5,7}"
prop_delta_u      = prop1 "delta({9,4,1,0,1,4,9}) == {-5,-3,-1,1,3,5}"
prop_mobius_u     = prop1 "mobius({1,3,4,7,6,12}) == {1,2,3,4,5,6}"
prop_mobiusi_u    = prop1 "mobiusi({1,2,3,4,5,6}) == {1,3,4,7,6,12}"
prop_dirichleti_u = prop1 "dirichleti({1,1,1,1,1,1,1,1}) == {1,-1,-1,0,-1,1,-1,0}"
prop_euler_u      = prop1 "euler({1,1,0,0,0,0,0}) == {1,2,2,3,3,4,4}"
prop_euleri_u     = prop1 "euleri({1,2,2,3,3,4,4}) == {1,1,0,0,0,0,0}"
prop_mset_u       = prop1 "mset({0,1,0,1}) == {1,1,1,2}"
prop_prods_u      = prop1 "prods({1,2,3,4,5}) == {1,2,6,24,120}"
prop_pset_u       = prop1 "pset({0,2,1}) == {1,2,2}"
prop_seq_u        = prop1 "seq({0,1,1,0,0,0}) == {1,1,2,3,5,8}"
prop_trisect0_u   = prop1 "trisect0({0,1,2,3,4,5,6}) == {0,3,6}"
prop_trisect1_u   = prop1 "trisect1({0,1,2,3,4,5,6}) == {1,4}"
prop_trisect2_u   = prop1 "trisect2({0,1,2,3,4,5,6}) == {2,5}"
prop_point_u      = prop1 "point({1,1,4,27,256}) == {0,1,8,81,1024}"
prop_weight_u     = prop1 "weight({1,1,1,1,1,1,1,1}) == {1,1,2,2,3,4,5,6}"
prop_partition_u  = prop1 "partition({1,3,5}) == [1,1,1,2,2,3,4,4,5,6,7,8,9,10,11,13,14,15,17,18]"
prop_hankel_u     = prop1 "hankel({6,5,4,3,2,1}) == {6,-1,0,0}"
prop_indicator_u  = prop1 "indicator({2,4}) == [0,0,1,0,1]"
prop_indicatorc_u = prop1 "indicatorc({2,4}) == [1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]"

prop_shift                    f  = prop  [f::S20] "shift(f) == diff(f./{n!}) .* {n!}"
prop_bousi_bous         (Full f) = prop  [f::S20] "f == bousi(bous(f))"
prop_bous_bousi         (Full f) = prop  [f::S20] "f == bous(bousi(f))"
prop_euleri_euler       (Full f) = prop' [f::S5]  "f == euleri(euler(f))"
prop_euler_euleri       (Full f) = prop' [f::S5]  "f == euler(euleri(f))"
prop_mobiusi_mobius     (Full f) = prop  [f::S10] "f == mobiusi(mobius(f))"
prop_mobius_mobiusi     (Full f) = prop  [f::S10] "f == mobius(mobiusi(f))"
prop_dirichlet_dirichleti1 (FullInvertible f) = prop  [f::S20] "1 == dirichlet(f,dirichleti(f))"
prop_dirichlet_dirichleti2 (FullInvertible f) = prop  [f::S20] "1 == dirichlet(dirichleti(f),f)"
prop_Compose1           (Full f) = prop  [f::S20] "f == x@f"
prop_Compose2           (Full f) = prop  [f::S20] "f == f@x"

prop_delta  f = prop' [f::S20] "delta(f) == (diff(f./{n!}) .* {n!} - f)"
prop_point f = prop  [f::S20] "point(f) == x*diff(f./{n!}) .* {n!}"

prop_Distrib1 :: S20 -> S20 -> S20 -> Bool
prop_Distrib1 f g h = f*(g+h) ~== f*g + f*h

prop_Distrib2 :: S20 -> S20 -> S20 -> Bool
prop_Distrib2 f g h = f.*(g+h) ~== (f.*g + f.*h)

prop_Distrib3 :: S20 -> S20 -> S20 -> Bool
prop_Distrib3 f g h = g./f + h./f ~== (g+h)./f

prop_Reflexivity f = prop [f::Series 1000] "f == f"

prop_Powers_of_2 f = prop [f::Series 30] "{2^n} == 1/(1-2*x)"

dropTrailingZeros = reverse . dropWhile (== 0) . reverse

prop_ListOfCoeffs f = runExpr1 (pretty f) == (f :: Series 100)

prop_Polynomial (Full f) = runExpr1 p == (f :: S10)
  where
    cs = coeffList f
    term c i = B.concat [pretty c, "*x^", pretty i]
    p = B.intercalate "+" (zipWith term cs [0::Integer ..])

-- Arithmetic progression
prop_AP a b = ogf20 [a,b..] == runExpr1 (B.concat ["{", pretty a, ",", pretty b, ",...}"])

prop_Geometric1 c = prop [ogf20 [c^i | i<-[0..]]] (B.pack $ printf "f == {(%s)^n}" (show c))
prop_Geometric2 c = prop [ogf20 [c^i | i<-[0..]]] (B.pack $ printf "f == 1/(1 - %s*x)" (show c))

prop_Connected_labeled_graphs = prop1 "f=A006125-1;1+{0,(-1)^(n+1)/n}@(f./{n!}).*{n!} == A001187"

prop_D_of_geometric = prop1' "diff(1/(1-x)) == (1/(1-x))^2"

prop_Product_rule (Full1 f) (Full1 g) = prop' [f,g::S20] "diff(f*g) == diff(f)*g + f*diff(g)"
prop_Chain_rule (Revertible f) g      = prop' [f,g::S20] "diff(g@f) == (diff(g))@f * diff(f)"
prop_Reciprocal_rule (FullUnit f)     = prop  [f::S20]   "diff(1/f) == -diff(f)/f^2"

-- Fundametal Theorem of Calculus
prop_Fundamental1 f = prop' [f::S20] "f == f(0) + int(diff(f))"
prop_Fundamental2 f = prop' [f::S20] "f == diff(int(f))"

-- Integration by parts
prop_Integration_by_parts f g =
    prop [f,g::S20] "f*g == (f*g)@0 + int(diff(f)*g) + int(f*diff(g))"

prop_IncreasingForests = prop1' "tree=int(forest);forest=exp(tree) == 1/(1-x)"

prop_Unit_circle (FullRevertible1 f) = prop' [f::S10] "cos(f)^2 + sin(f)^2 == 1"

prop_Exact_sqrt c d =
    let prg = B.pack ("sqrt({(" ++ show c ++ ")^2/(" ++ show d ++ ")^2})")
    in runExpr1 prg ~= ogf20 [abs c::Integer] / ogf20 [abs d::Integer]

prop_sinh        (Revertible1 f) = prop' [f::S10] "sinh(f)      == (exp(f)-exp(-f))/2"
prop_cosh    (FullRevertible1 f) = prop' [f::S10] "cosh(f)      == (exp(f)+exp(-f))/2"
prop_tanh        (Revertible1 f) = prop' [f::S10] "tanh(f)      == (exp(f)-exp(-f))/(exp(f)+exp(-f))"
prop_arsinh      (Revertible1 f) = prop' [f::S10] "arsinh(f)    == log(f + sqrt(f^2 + 1))"
prop_artanh      (Revertible1 f) = prop' [f::S10] "artanh(f)    == (log(1+f) - log(1-f))/2"
prop_D_of_sin    (Revertible f)  = prop' [f::S10] "diff(sin(f))    == diff(f)*cos(f)"
prop_D_of_cos    (Revertible f)  = prop' [f::S10] "diff(cos(f))    == -diff(f)*sin(f)"
prop_D_of_tan    (Revertible f)  = prop' [f::S10] "diff(tan(f))    == diff(f)/cos(f)^2"
prop_D_of_arcsin (Revertible f)  = prop' [f::S10] "diff(arcsin(f)) == diff(f)/sqrt(1-f^2)"
prop_D_of_arccos (Revertible1 f) = prop' [f::S10] "diff(arccos(f)) == -diff(f)/sqrt(1-f^2)"
prop_D_of_arctan (Revertible1 f) = prop' [f::S10] "diff(arctan(f)) == diff(f)/(1+f^2)"
prop_D_of_sinh   (Revertible1 f) = prop' [f::S10] "diff(sinh(f))   == diff(f)*cosh(f)"
prop_D_of_cosh   (Revertible1 f) = prop' [f::S10] "diff(cosh(f))   == diff(f)*sinh(f)"
prop_D_of_tanh   (Revertible1 f) = prop' [f::S10] "diff(tanh(f))   == diff(f)*(1-tanh(f)^2)"
prop_D_of_arsinh (Revertible1 f) = prop' [f::S10] "diff(arsinh(f)) == diff(f)/sqrt(1+f^2)"
prop_D_of_arcosh (Revertible1 f) = prop' [f::S10] "diff(arcosh(f)) == diff(f)/sqrt(f^2-1)"
prop_D_of_artanh (Revertible1 f) = prop' [f::S10] "diff(artanh(f)) == diff(f)/(1-f^2)"
prop_Hyperbolic_unit (FullRevertible1 f) = prop' [f::S10] "cosh(f)^2 - sinh(f)^2 == 1"

prop_Labeled_trees_1  = prop1 "A000272 == {1,1,n^(n-2)}"
prop_Labeled_trees_2  = prop1 "A000272 == T=x*exp(T);F=1+T-(1/2)*T^2;F.*{n!}"
prop_Unlabeled_trees  = prop1 "f=A000081;1+f-f^2/2+f(x^2)/2 == A000055"
prop_Labeled_graphs   = propN1 16 "A006125 == {2^(n*(n-1)/2)}"
prop_Fredholm_Rueppel = prop1 "A036987 == F=1+x*F(x^2)"

prop_Bell_1      = prop1  "A000110 == {1/n!}@{0,1/n!}.*{n!}"
prop_Bell_2      = prop1  "A000110 == e=1+int(e);e(e-1).*{n!}"
prop_Bell_3      = prop1  "A000110 == y=x/(1-x);B=1+y*B(y)"
prop_Bell_4      = prop1  "A000110 == Bell=1+int(exp(x)*Bell); Bell .* {n!}"
prop_Bessel      = prop1  "A006789 == B=1/(1-x-x^2*B(x/(1-x))/(1-x))"
prop_A000670_1   = prop1  "A000670 == 1/(2-exp(x)).*{n!}"
prop_A000670_2   = prop1  "A000670 == y=1+int(2*y^2-y);y.*{n!}"
prop_A000670_3   = prop1  "A000670 == f={(n+1)!};STIR=((x*f./{n!})@({0,1/n!}).*{n!})/x;1+x*STIR"
prop_A000670_4   = prop1  "A000670 == A=int(1+3*A+2*A^2);(1+A).*{n!}"
prop_Catalan_1   = prop1  "A000108 == {(2*n)!/(n!*(n+1)!)}"
prop_Catalan_2   = prop1' "A000108 == (1-sqrt(1-4*x))/(2*x)"
prop_Catalan_3   = propN1 10 "A000108 == 1/(1-x/(1-x/(1-x/(1-x/(1-x/(1-x/(1-x/(1-x/(1-x)))))))))"
prop_Catalan_4   = prop1 "A000108 == C=1+x*C^2"
prop_Motzkin_1   = propN1 (-2) "A001006 == (1-x-(1-2*x-3*x^2)^(1/2))/(2*x^2)"
prop_Motzkin_2   = prop1' "f=A000108;(f(x/(1+x))-1)/x == A001006"
prop_Motzkin_3   = propN1 11 "A001006 == 1/(1-x-x^2/(1-x-x^2/(1-x-x^2/(1-x-x^2/(1-x-x^2)))))"
prop_Motzkin_4   = prop1  "A001006 == M=1+x*M+x^2*M^2"
prop_Bous_1      = prop1  "A000111 == bous([1])"
prop_Bous_2      = prop1  "A000667 == -bous(-1/(1-x))"
prop_Bous_3      = prop1  "A062162 == bous(1/(1+x))"
prop_Euler       = prop1  "A122045 == 2/({1/n!}+{(-1)^n/n!}).*{n!}"
prop_Fibonacci_1 = prop1' "shift(A000045) == 1/(1-x-x^2)"
prop_Fibonacci_2 = prop1' "shift(A000045) == F=1+(x+x^2)*F"
prop_A001710_1   = prop1  "A001710 == (2-x^2)/(2-2*x).*{n!}"
prop_A001710_2   = prop1  "A001710 == {1,1,n!/2}"
prop_A235802     = prop1  "A235802 == 1/(1-x)^(2/(2-x)).*{n!}"
prop_A075834     = prop1  "A075834 == A=1+x/(1-x*diff(A)/A)"
prop_A003149     = prop1' "A003149 == (log(1-x)/(x/2-1).*{n!})/x"
prop_A012259_1   = prop1  "A012259 == exp(artanh(tan(x))).*{n!}"
prop_A012259_2   = prop1  "A012259 == sqrt(sec(2*x)+tan(2*x)).*{n!}"
prop_A012259_3   = prop1  "A012259 == sqrt((1+tan(x))/(1-tan(x))).*{n!}"
prop_A202152     = prop1  "A202152 == exp(x*(1+x)^x).*{n!}"
prop_A191422     = prop1  "A191422 == (1+x+x^2)^x.*{n!}"
prop_A088789     = prop1  "A088789 == F=2*x/(1+exp(x));laplace(revert(F))"
prop_A049140     = prop1' "A049140 == shift(revert(x*(1-x-x^3)))"
prop_A008965     = prop1' "A008965 == cyc(indicator({n+1}))/x"

prop_Hankel_A000108 = prop1' "hankel(A000108) == {1,1,1,1,1,1,1,1,1,1}"
prop_Hankel_A033321 = prop1' "hankel(A033321) == {1,1,1,1,1,1,1,1,1,1}"

-- Naive cofactor implementation of determinant
determinant :: Num a => Vector (Vector a) -> a
determinant m =
  let n = V.length m
      minor i j = del j . V.map (del i)
      del k u = let (s, t) = V.splitAt k u in V.concat[s, V.tail t]
  in if V.null m
     then 1
     else let c = V.head m
          in sum [ (-1)^i * c!i * determinant (minor i 0 m)
                 | i <- [0..n-1]
                 ]

prop_Determinant =
    forAll (resize 6 arbitrary) $ \m ->
        det m == determinant (m :: Matrix Rational)

prop_Coefficients =
    prop [f] "f == (1/(1-[0,1,1]))?[2*n+1]" &&
    prop [f] "f == (1/(1-[0,1,1]))?{2*n+1}" &&
    prop [a] "f == (1/(1-[0,1,1]))?5"
  where
    f = ogf20 [1,3,8,21,55,144,377,987,2584,6765]
    a = ogf20 [8]

prop_seq (Revertible f) g = prop [f,g::Series 30] "seq(f) == indicator(g)@f + indicatorc(g)@f"

mset :: KnownNat n => Series n -> Series n
mset f
  | constant f /= 0 = infty
  | otherwise =
      let term k = (f `o` xpow k) / fromIntegral k
      in exp $ sum $ term <$> [1 .. precision f - 1]

pset :: KnownNat n => Series n -> Series n
pset f
  | constant f /= 0 = infty
  | otherwise =
      let term k = (-1)^(k+1) * (f `o` xpow k) / fromIntegral k
      in exp $ sum $ term <$> [1 .. precision f - 1]

prop_mset (Revertible f) = uncurry (~=) $ evalEqn [f::S10, mset f] "mset(f) == g"
prop_pset (Revertible f) = uncurry (~=) $ evalEqn [f::S10, pset f] "pset(f) == g"

lookupTr1 :: KnownNat n => ByteString -> Series n -> Series n
lookupTr1 name =
    let Just (Transform 1 tr) = lookupTransform name
    in \f -> tr [f]

equalTr :: KnownNat n => ByteString -> ByteString -> Series n -> Bool
equalTr name1 name2 f = lookupTr1 name1 f ~= lookupTr1 name2 f

lessTr :: KnownNat n => ByteString -> ByteString -> Series n -> Bool
lessTr name1 name2 f = lookupTr1 name1 f ~< lookupTr1 name2 f

prop_Stieltjes_Poly :: KnownNat n => Series n -> Bool
prop_Stieltjes_Poly = equalTr "stieltjesPoly" "stieltjesReference"

prop_Jacobi0_Poly :: KnownNat n => Series n -> Bool
prop_Jacobi0_Poly = equalTr "jacobiPoly0" "jacobiReference0"

prop_Jacobi1_Poly :: KnownNat n => Series n -> Bool
prop_Jacobi1_Poly = equalTr "jacobiPoly1" "jacobiReference1"

prop_Stieltjes_Hankel :: KnownNat n => Series n -> Bool
prop_Stieltjes_Hankel = equalTr "stieltjes" "stieltjesReference"

prop_Jacobi0_Hankel :: KnownNat n => Series n -> Bool
prop_Jacobi0_Hankel = equalTr "jacobi0" "jacobiReference0"

prop_Jacobi1_Hankel :: KnownNat n => Series n -> Bool
prop_Jacobi1_Hankel = equalTr "jacobi1" "jacobiReference1"

prop_Stieltjes_QD :: KnownNat n => Series n -> Bool
prop_Stieltjes_QD = lessTr "stieltjesQD" "stieltjesReference"

prop_Jacobi0_QD :: KnownNat n => Series n -> Bool
prop_Jacobi0_QD f@(Series v) = lessTr "jacobiQD0" "jacobiReference0" f

prop_Jacobi1_QD :: KnownNat n => Series n -> Bool
prop_Jacobi1_QD = lessTr "jacobiQD1" "jacobiReference1"

tests =
    [ ("Expr/associative",        check 100 prop_Expr_assoc)
    , ("Expr/value",              check 100 prop_Expr_value)
    , ("unit/Rat-Power",          check   1 prop_Rat_power_u)
    , ("unit/Neg-Power",          check   1 prop_Neg_power_u)
    , ("unit/shift",              check   1 prop_shift_u)
    , ("unit/bisect0",            check   1 prop_bisect0_u)
    , ("unit/bisect1",            check   1 prop_bisect1_u)
    , ("unit/cyc",                check   1 prop_cyc_u)
    , ("unit/delta",              check   1 prop_delta_u)
    , ("unit/mobius",             check   1 prop_mobius_u)
    , ("unit/mobiusi",            check   1 prop_mobiusi_u)
    , ("unit/dirichleti",         check   1 prop_dirichleti_u)
    , ("unit/bous",               check   1 prop_bous_u)
    , ("unit/bousi",              check   1 prop_bousi_u)
    , ("unit/euler",              check   1 prop_euler_u)
    , ("unit/euleri",             check   1 prop_euleri_u)
    , ("unit/mset",               check   1 prop_mset_u)
    , ("unit/pset",               check   1 prop_pset_u)
    , ("unit/prods",              check   1 prop_prods_u)
    , ("unit/trisect0",           check   1 prop_trisect0_u)
    , ("unit/trisect1",           check   1 prop_trisect1_u)
    , ("unit/trisect2",           check   1 prop_trisect2_u)
    , ("unit/point",              check   1 prop_point_u)
    , ("unit/weight",             check   1 prop_weight_u)
    , ("unit/partition",          check   1 prop_partition_u)
    , ("unit/hankel",             check   1 prop_hankel_u)
    , ("unit/indicator",          check   1 prop_indicator_u)
    , ("unit/indicatorc",         check   1 prop_indicatorc_u)
    , ("shift",                   check 100 prop_shift)
    , ("bousi.bous=id",           check  20 prop_bousi_bous)
    , ("bous.bousi=id",           check  20 prop_bous_bousi)
    , ("euleri.euler=id",         check   5 prop_euleri_euler)
    , ("euler.euleri=id",         check   5 prop_euler_euleri)
    , ("mobiusi_mobius=id",       check 100 prop_mobiusi_mobius)
    , ("mobius.mobiusi=id",       check 100 prop_mobius_mobiusi)
    , ("dirichlet.dirichleti1=1", check 100 prop_dirichlet_dirichleti1)
    , ("dirichlet.dirichleti2=1", check 100 prop_dirichlet_dirichleti2)
    , ("delta = expr",            check 100 prop_delta)
    , ("point = x*diff(f./{n!}).*{n!}", check 100 prop_point)
    , ("distributive1",           check 100 prop_Distrib1)
    , ("distributive2",           check 100 prop_Distrib2)
    , ("distributive3",           check 100 prop_Distrib3)
    , ("reflexivity",             check 100 prop_Reflexivity)
    , ("eval: [2^n]",             check 100 prop_Powers_of_2)
    , ("eval: x@f=f",             check 100 prop_Compose1)
    , ("eval: f@x=f",             check 100 prop_Compose2)
    , ("Bell-1",                  check   1 prop_Bell_1)
    , ("Bell-2",                  check   1 prop_Bell_2)
    , ("Bell-3",                  check   1 prop_Bell_3)
    , ("Bell-4",                  check   1 prop_Bell_4)
    , ("Bessel",                  check   1 prop_Bessel)
    , ("Catalan-1",               check   1 prop_Catalan_1)
    , ("Catalan-2",               check   1 prop_Catalan_2)
    , ("Catalan-3",               check   1 prop_Catalan_3)
    , ("Catalan-4",               check   1 prop_Catalan_4)
    , ("Motzkin-1",               check   1 prop_Motzkin_1)
    , ("Motzkin-2",               check   1 prop_Motzkin_2)
    , ("Motzkin-3",               check   1 prop_Motzkin_3)
    , ("Motzkin-4",               check   1 prop_Motzkin_4)
    , ("Boustrophedon-1",         check   1 prop_Bous_1)
    , ("Boustrophedon-2",         check   1 prop_Bous_2)
    , ("Boustrophedon-3",         check   1 prop_Bous_3)
    , ("Euler",                   check   1 prop_Euler)
    , ("Fibonacci-1",             check   1 prop_Fibonacci_1)
    , ("Fibonacci-2",             check   1 prop_Fibonacci_2)
    , ("List of coeffs",          check 100 prop_ListOfCoeffs)
    , ("Polynomial",              check  50 prop_Polynomial)
    , ("Arithmetic progression",  check 100 prop_AP)
    , ("Geometric series 1",      check 100 prop_Geometric1)
    , ("Geometric series 2",      check 100 prop_Geometric2)
    , ("Labeled trees 1",         check   1 prop_Labeled_trees_1)
    , ("Labeled trees 2",         check   1 prop_Labeled_trees_2)
    , ("Unlabeled trees",         check   1 prop_Unlabeled_trees)
    , ("Labeled graphs",          check   1 prop_Labeled_graphs)
    , ("Connected labeled graphs", check 1 prop_Connected_labeled_graphs)
    , ("Derivative of geometric",  check 1 prop_D_of_geometric)
    , ("Product rule",            check  50 prop_Product_rule)
    , ("Reciprocal rule",         check  50 prop_Reciprocal_rule)
    , ("Chain rule",              check  50 prop_Chain_rule)
    , ("Fund Thm of Calc 1",      check 100 prop_Fundamental1)
    , ("Fund Thm of Calc 2",      check 100 prop_Fundamental2)
    , ("Integration by parts",    check 100 prop_Integration_by_parts)
    , ("A235802",                 check   1 prop_A235802)
    , ("A001710-1",               check   1 prop_A001710_1)
    , ("A001710-2",               check   1 prop_A001710_2)
    , ("A075834",                 check   1 prop_A075834)
    , ("A003149",                 check   1 prop_A003149)
    , ("Fredholm-Rueppel",        check   1 prop_Fredholm_Rueppel)
    , ("A000670-1",               check   1 prop_A000670_1)
    , ("A000670-2",               check   1 prop_A000670_2)
    , ("A000670-3",               check   1 prop_A000670_3)
    , ("A000670-4",               check   1 prop_A000670_4)
    , ("Increasing forests",      check   1 prop_IncreasingForests)
    , ("Unit circle",             check  20 prop_Unit_circle)
    , ("Exact sqrt",              check  50 prop_Exact_sqrt)
    , ("Derivative of sin",       check  50 prop_D_of_sin)
    , ("Derivative of cos",       check  50 prop_D_of_cos)
    , ("Derivative of tan",       check  50 prop_D_of_tan)
    , ("Derivative of arcsin",    check  50 prop_D_of_arcsin)
    , ("Derivative of arccos",    check  50 prop_D_of_arccos)
    , ("Derivative of arctan",    check  50 prop_D_of_arctan)
    , ("Derivative of sinh",      check  50 prop_D_of_sinh)
    , ("Derivative of cosh",      check  50 prop_D_of_cosh)
    , ("Derivative of tanh",      check  50 prop_D_of_tanh)
    , ("Derivative of arsinh",    check   5 prop_D_of_arsinh)
    , ("Derivative of arcosh",    check   5 prop_D_of_arcosh)
    , ("Derivative of artanh",    check  50 prop_D_of_artanh)
    , ("sinh",                    check  50 prop_sinh)
    , ("cosh",                    check  50 prop_cosh)
    , ("tanh",                    check  50 prop_tanh)
    , ("Hyperbolic unit",         check  50 prop_Hyperbolic_unit)
    , ("arsinh",                  check  50 prop_arsinh)
    , ("artanh",                  check  50 prop_artanh)
    , ("A012259-1",               check   1 prop_A012259_1)
    , ("A012259-2",               check   1 prop_A012259_2)
    , ("A012259-3",               check   1 prop_A012259_3)
    , ("A202152",                 check   1 prop_A202152)
    , ("A191422",                 check   1 prop_A191422)
    , ("A088789",                 check   1 prop_A088789)
    , ("A049140",                 check   1 prop_A049140)
    , ("A008965",                 check   1 prop_A008965)
    , ("hankel(A000108)",         check   1 prop_Hankel_A000108)
    , ("hankel(A033321)",         check   1 prop_Hankel_A033321)
    , ("Determinant",             check 100 prop_Determinant)
    , ("Coefficients",            check   1 prop_Coefficients)
    , ("seq identity",            check  50 prop_seq)
    , ("mset identity",           check  50 prop_mset)
    , ("pset identity",           check  50 prop_pset)
    , ("Stieltjes/Poly/7",        check 100 (prop_Stieltjes_Poly :: Series 7 -> Bool))
    , ("Stieltjes/Poly/8",        check 100 (prop_Stieltjes_Poly :: Series 8 -> Bool))
    , ("Jacobi0/Poly/7",          check 100 (prop_Jacobi0_Poly :: Series 7 -> Bool))
    , ("Jacobi0/Poly/8",          check 100 (prop_Jacobi0_Poly :: Series 8 -> Bool))
    , ("Jacobi1/Poly/7",          check 100 (prop_Jacobi1_Poly :: Series 7 -> Bool))
    , ("Jacobi1/Poly/8",          check 100 (prop_Jacobi1_Poly :: Series 8 -> Bool))
    , ("Stieltjes/Hankel/7",      check 100 (prop_Stieltjes_Hankel :: Series 7 -> Bool))
    , ("Stieltjes/Hankel/8",      check 100 (prop_Stieltjes_Hankel :: Series 8 -> Bool))
    , ("Jacobi0/Hankel/7",        check 100 (prop_Jacobi0_Hankel :: Series 7 -> Bool))
    , ("Jacobi0/Hankel/8",        check 100 (prop_Jacobi0_Hankel :: Series 8 -> Bool))
    , ("Jacobi1/Hankel/7",        check 100 (prop_Jacobi1_Hankel :: Series 7 -> Bool))
    , ("Jacobi1/Hankel/8",        check 100 (prop_Jacobi1_Hankel :: Series 8 -> Bool))
    , ("Stieltjes/QD/7",          check 100 (prop_Stieltjes_QD :: Series 7 -> Bool))
    , ("Stieltjes/QD/8",          check 100 (prop_Stieltjes_QD :: Series 8 -> Bool))
    -- , ("Jacobi0/QD/7",            check 100 (prop_Jacobi0_QD :: Series 7 -> Bool))
    -- , ("Jacobi0/QD/8",            check 100 (prop_Jacobi0_QD :: Series 8 -> Bool))
    -- , ("Jacobi1/QD/7",            check 100 (prop_Jacobi1_QD :: Series 7 -> Bool))
    -- , ("Jacobi1/QD/8",            check 100 (prop_Jacobi1_QD :: Series 8 -> Bool))
    ]

main =
    forM_ tests $ \(name, chk) -> do
        putStr (name ++ ": ")
        result <- chk
        unless (isSuccess result) exitFailure
