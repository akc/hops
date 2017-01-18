{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Copyright   : Anders Claesson 2015, 2016
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

import GHC.TypeLits
import Text.Printf
import Data.List
import Data.Maybe
import Data.Monoid
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

type S5  = Series 5
type S10 = Series 10
type S20 = Series 20

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

instance KnownNat n => Arbitrary (Revertible n) where
    arbitrary = Revertible . series (Proxy :: Proxy n) . (0:) <$> arbitrary

instance KnownNat n => Arbitrary (Unit n) where
    arbitrary = Unit . series (Proxy :: Proxy n) <$> ((:) <$> nonzeroValGen <*> arbitrary)

nonzeroVal :: Rat -> Bool
nonzeroVal (Val r) | r /= 0 = True
nonzeroVal _ = False

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

instance Arbitrary Prg where
    arbitrary = Prg <$> listOf arbitrary

instance Arbitrary Cmd where
    arbitrary = oneof
        [ Expr  <$> arbitrary
        , Asgmt <$> nameGen <*> arbitrary
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
        , ( 1, Expr0  <$> arbitrary)
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

infix 4 ~==

f ~== g = as `isPrefixOf` bs || bs `isPrefixOf` as
  where
    as = rationalPrefix f
    bs = rationalPrefix g

check :: Testable prop => Int -> prop -> IO Result
check n = quickCheckWithResult stdArgs {maxSuccess = n}

evalPrg :: KnownNat n => Env n -> Prg -> Series n
evalPrg env = evalCorePrg env . core

evalPrg1 :: KnownNat n => Prg -> Series n
evalPrg1 = evalCorePrg emptyEnv . core

runPrg :: KnownNat n => Env n -> ByteString -> Series n
runPrg env = evalPrg env . fromMaybe (error "parse error") . parsePrg

runPrg1 :: KnownNat n => ByteString -> Series n
runPrg1 = runPrg emptyEnv

ogf :: KnownNat n => Proxy n -> [Integer] -> Series n
ogf n = series n . map (Val . fromIntegral)

ogf20 = ogf (Proxy :: Proxy 20)

stubDB :: KnownNat n => Vector (Series n)
stubDB = nilVec // [(i-1, series' xs) | (ANum i, xs) <- stubDBaList]
  where
    nilVec  = V.replicate 555555 nil
    series' = series (Proxy :: Proxy n) . map Val

stubDBaList :: [(ANum, Sequence)]
stubDBaList = unsafePerformIO (parseStripped <$> B.readFile "tests/stub.db")
{-# NOINLINE stubDBaList #-}

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
    exec = runPrg $ Env stubDB (M.fromList (zip nameSupply fs))

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

prop_Prg_id1 p = mempty <> p == (p :: Prg)
prop_Prg_id2 p = p <> mempty == (p :: Prg)
prop_Prg_assoc p q r = p <> (q <> r) == (p <> q) <> (r :: Prg)

prop_Prg_value p = evalPrg1 p == (evalPrg1 q :: Series 40)
  where
    q = p <> fromJust (parsePrg "stdin") :: Prg

prop_Rat_power_u = (1/4) !^! (3/2) == Val (1 % 8)
prop_Neg_power_u = prop1 "{-(-1)^n} == -1/(1+x)"

prop_LEFT_u      = prop1 "LEFT      {4,3,2,1}          == {3,2,1}"
prop_RIGHT_u     = prop1 "RIGHT     {4,3,2,1}          == {1,4,3,2,1}"
prop_BINOMIAL_u  = prop1 "BINOMIAL  {1,2,4,8,16}       == {1,3,9,27,81}"
prop_BINOMIALi_u = prop1 "BINOMIALi {1,3,9,27,81}      == {1,2,4,8,16}"
prop_BIN1_u      = prop1 "BIN1      {2,4,8,16}         == {2,-8,26,-80}"
prop_BISECT0_u   = prop1 "BISECT0   {0,1,2,3,4,5}      == {0,2,4}"
prop_BISECT1_u   = prop1 "BISECT1   {0,1,2,3,4,5}      == {1,3,5}"
prop_BOUS_u      = prop1 "BOUS      {5,4,3,2,1}        == {5,9,16,33,84}"
prop_BOUSi_u     = prop1 "BOUSi     {5,4,3,2,1}        == {5,-1,0,-5,4}"
prop_CATALAN_u   = prop1 "CATALAN   {1,1,1,1,1}        == {1,1,2,5,14}"
prop_CATALANi_u  = prop1 "CATALANi  {1,1,2,5,14}       == {1,1,1,1,1}"
prop_CYC_u       = prop1 "CYC       {0,1,1,1,1,1}      == {0,1,2,3,5,7}"
prop_DIFF_u      = prop1 "DIFF      {9,4,1,0,1,4,9}    == {-5,-3,-1,1,3,5}"
prop_MOBIUS_u    = prop1 "MOBIUS    {1,3,4,7,6,12}     == {1,2,3,4,5,6}"
prop_MOBIUSi_u   = prop1 "MOBIUSi   {1,2,3,4,5,6}      == {1,3,4,7,6,12}"
prop_DIRICHLETi_u= prop1 "DIRICHLETi{1,1,1,1,1,1,1,1}  == {1,-1,-1,0,-1,1,-1,0}"
prop_EULER_u     = prop1 "EULER     {1,1,0,0,0,0,0}    == {1,2,2,3,3,4,4}"
prop_EULERi_u    = prop1 "EULERi    {1,2,2,3,3,4,4}    == {1,1,0,0,0,0,0}"
prop_LAH_u       = prop1 "LAH       {5,4,3,2,1}        == {5,4,11,44,229}"
prop_LAHi_u      = prop1 "LAHi      {5,4,3,2,1}        == {5,4,-5,8,-11}"
prop_EXP_u       = prop1 "EXP       {1,2,3,4}          == {1,3,10,41}"
prop_LOG_u       = prop1 "LOG       {1,3,10,41}        == {1,2,3,4}"
prop_MSET_u      = prop1 "MSET      {0,1,0,1}          == {1,1,1,2}"
prop_PRODS_u     = prop1 "PRODS     {1,2,3,4,5}        == {1,2,6,24,120}"
prop_PSET_u      = prop1 "PSET      {0,2,1}            == {1,2,2}"
prop_SEQ_u       = prop1 "SEQ       {0,1,1,0,0,0}      == {1,1,2,3,5,8}"
prop_STIRLING_u  = prop1 "STIRLING  {1,2,3,4,5}        == {1,3,10,37,151}"
prop_STIRLINGi_u = prop1 "STIRLINGi {1,3,10,37,151}    == {1,2,3,4,5}"
prop_TRISECT0_u  = prop1 "TRISECT0  {0,1,2,3,4,5,6}    == {0,3,6}"
prop_TRISECT1_u  = prop1 "TRISECT1  {0,1,2,3,4,5,6}    == {1,4}"
prop_TRISECT2_u  = prop1 "TRISECT2  {0,1,2,3,4,5,6}    == {2,5}"
prop_POINT_u     = prop1 "POINT     {1,1,4,27,256}     == {0,1,8,81,1024}"
prop_WEIGHT_u    = prop1 "WEIGHT    {1,1,1,1,1,1,1,1}  == {1,1,2,2,3,4,5,6}"
prop_PARTITION_u = prop1 "PARTITION {1,3,5} == [1,1,1,2,2,3,4,4,5,6,7,8,9,10,11,13,14,15,17,18]"
prop_HANKEL_u    = prop1 "HANKEL    {6,5,4,3,2,1}      == {6,-1,0,0}"
prop_lHANKEL_u   = prop1 "lHANKEL   {1,4,9,16,25,36}   == {7,17,31,49}"
prop_I_u         = prop1 "I         {2,4}              == [0,0,1,0,1]"
prop_IC_u        = prop1 "IC {2,4} == [1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]"

prop_LEFT                     f  = prop  [f::S20] "LEFT(f) == D(f./{n!}) .* {n!}"
prop_RIGHT             (NonDZ f) = prop  [f::S20] "RIGHT(f) == 1 + x*f"
prop_BINOMIALi_BINOMIAL (Full f) = prop  [f::S20] "f == BINOMIALi(BINOMIAL(f))"
prop_BINOMIAL_BINOMIALi (Full f) = prop  [f::S20] "f == BINOMIAL(BINOMIALi(f))"
prop_BOUSi_BOUS         (Full f) = prop  [f::S20] "f == BOUSi(BOUS(f))"
prop_BOUS_BOUSi         (Full f) = prop  [f::S20] "f == BOUS(BOUSi(f))"
prop_CATALANi_CATALAN   (Full f) = prop  [f::S20] "f == CATALANi(CATALAN(f))"
prop_CATALAN_CATALANi   (Full f) = prop  [f::S20] "f == CATALAN(CATALANi(f))"
prop_LAHi_LAH           (Full f) = prop  [f::S10] "f == LAHi(LAH(f))"
prop_LAH_LAHi           (Full f) = prop  [f::S10] "f == LAH(LAHi(f))"
prop_EULERi_EULER       (Full f) = prop' [f::S5]  "f == EULERi(EULER(f))"
prop_EULER_EULERi       (Full f) = prop' [f::S5]  "f == EULER(EULERi(f))"
prop_LOG_EXP            (Full f) = prop' [f::S10] "f == LOG(EXP(f))"
prop_EXP_LOG            (Full f) = prop' [f::S10] "f == EXP(LOG(f))"
prop_MOBIUSi_MOBIUS     (Full f) = prop  [f::S10] "f == MOBIUSi(MOBIUS(f))"
prop_MOBIUS_MOBIUSi     (Full f) = prop  [f::S10] "f == MOBIUS(MOBIUSi(f))"
prop_DIRICHLET_DIRICHLETi1 (FullInvertible f) = prop  [f::S20] "1 == DIRICHLET(f,DIRICHLETi(f))"
prop_DIRICHLET_DIRICHLETi2 (FullInvertible f) = prop  [f::S20] "1 == DIRICHLET(DIRICHLETi(f),f)"
prop_STIRLINGi_STIRLING (Full f) = prop' [f::S10] "f == STIRLINGi(STIRLING(f))"
prop_STIRLING_STIRLINGi (Full f) = prop' [f::S10] "f == STIRLING(STIRLINGi(f))"
prop_BIN1_involutive    (Full f) = prop' [f::S10] "f == BIN1(BIN1(f))"
prop_Compose1           (Full f) = prop  [f::S20] "f == x@f"
prop_Compose2           (Full f) = prop  [f::S20] "f == f@x"

prop_BINOMIAL  f = prop  [f::S20] "BINOMIAL(f)  == (f ./ {n!}) * {1/n!} .* {n!}"
prop_BINOMIALi f = prop  [f::S20] "BINOMIALi(f) == (f ./ {n!}) * {(-1)^n/n!} .* {n!}"
prop_BIN1      f = prop  [f::S20] "BIN1(f)      == LEFT((-{(-1)^n/n!} * (((x*f) ./ {n!})@(-x))) .* {n!})"
prop_DIFF      f = prop' [f::S20] "DIFF(f)      == (D(f./{n!}) .* {n!} - f)"
prop_LAH       f = prop  [f::S20] "LAH(f)       == (f./{n!})@(x/(1-x)) .* {n!}"
prop_LAHi      f = prop  [f::S20] "LAHi(f)      == (f./{n!})@(x/(1+x)) .* {n!}"
prop_EXP       f = prop' [f::S20] "EXP(f)       == (({1/n!}@(x*f./{n!}) - 1) .* {n!})/x"
prop_LOG       f = prop' [f::S20] "LOG(f)       == ({0,(-1)^(n+1)/n}@(x*f./{n!}) .* {n!})/x"
prop_STIRLING  f = prop' [f::S20] "STIRLING(f)  == ((x*f ./ {n!})@({0,1/n!}) .* {n!})/x"
prop_STIRLINGi f = prop' [f::S20] "STIRLINGi(f) == ((x*f ./ {n!})@({0,(-1)^(n+1)/n}) .* {n!})/x"
prop_POINT     f = prop  [f::S20] "POINT(f)     == x*D(f./{n!}) .* {n!}"

prop_Distrib1 :: S20 -> S20 -> S20 -> Bool
prop_Distrib1 f g h = f*(g+h) ~== f*g + f*h

prop_Distrib2 :: S20 -> S20 -> S20 -> Bool
prop_Distrib2 f g h = f.*(g+h) ~== (f.*g + f.*h)

prop_Distrib3 :: S20 -> S20 -> S20 -> Bool
prop_Distrib3 f g h = g./f + h./f ~== (g+h)./f

prop_Reflexivity f = prop [f::Series 1000] "f == f"

prop_Powers_of_2 f = prop [f::Series 30] "{2^n} == 1/(1-2*x)"

dropTrailingZeros = reverse . dropWhile (== 0) . reverse

prop_ListOfCoeffs f = runPrg1 (pretty f) == (f :: Series 100)

prop_Polynomial (Full f) = runPrg1 p == (f :: S10)
  where
    cs = coeffList f
    term c i = B.concat [pretty c, "*x^", pretty i]
    p = B.intercalate "+" (zipWith term cs [0::Integer ..])

-- Arithmetic progression
prop_AP a b = ogf20 [a,b..] == runPrg1 (B.concat ["{", pretty a, ",", pretty b, ",...}"])

prop_Geometric1 c = prop [ogf20 [c^i | i<-[0..]]] (B.pack $ printf "f == {(%s)^n}" (show c))
prop_Geometric2 c = prop [ogf20 [c^i | i<-[0..]]] (B.pack $ printf "f == 1/(1 - %s*x)" (show c))

prop_Connected_labeled_graphs = prop1 "f=A006125-1;1+{0,(-1)^(n+1)/n}@(f./{n!}).*{n!} == A001187"

prop_D_of_geometric = prop1' "D(1/(1-x)) == (1/(1-x))^2"

prop_Product_rule (Full1 f) (Full1 g) = prop' [f,g::S20] "D(f*g) == D(f)*g + f*D(g)"
prop_Chain_rule (Revertible f) g      = prop' [f,g::S20] "D(g@f) == (D(g))@f * D(f)"
prop_Reciprocal_rule (FullUnit f)     = prop  [f::S20]   "D(1/f) == -D(f)/f^2"

-- Fundametal Theorem of Calculus
prop_Fundamental1 f = prop' [f::S20] "f == f(0) + integral(D(f))"
prop_Fundamental2 f = prop' [f::S20] "f == D(integral(f))"

-- Integration by parts
prop_Integration_by_parts f g =
    prop [f,g::S20] "f*g == (f*g)@0 + integral(D(f)*g) + integral(f*D(g))"

prop_IncreasingForests = prop1' "tree=integral(forest);forest=exp(tree) == 1/(1-x)"

prop_Unit_circle (FullRevertible1 f) = prop' [f::S10] "cos(f)^2 + sin(f)^2 == 1"

prop_Exact_sqrt c d =
    let prg = B.pack ("sqrt({(" ++ show c ++ ")^2/(" ++ show d ++ ")^2})")
    in runPrg1 prg ~= ogf20 [abs c::Integer] / ogf20 [abs d::Integer]

prop_sinh        (Revertible1 f) = prop' [f::S10] "sinh(f)      == (exp(f)-exp(-f))/2"
prop_cosh    (FullRevertible1 f) = prop' [f::S10] "cosh(f)      == (exp(f)+exp(-f))/2"
prop_tanh        (Revertible1 f) = prop' [f::S10] "tanh(f)      == (exp(f)-exp(-f))/(exp(f)+exp(-f))"
prop_arsinh      (Revertible1 f) = prop' [f::S10] "arsinh(f)    == log(f + sqrt(f^2 + 1))"
prop_artanh      (Revertible1 f) = prop' [f::S10] "artanh(f)    == (log(1+f) - log(1-f))/2"
prop_D_of_sin    (Revertible f)  = prop' [f::S10] "D(sin(f))    == D(f)*cos(f)"
prop_D_of_cos    (Revertible f)  = prop' [f::S10] "D(cos(f))    == -D(f)*sin(f)"
prop_D_of_tan    (Revertible f)  = prop' [f::S10] "D(tan(f))    == D(f)/cos(f)^2"
prop_D_of_arcsin (Revertible f)  = prop' [f::S10] "D(arcsin(f)) == D(f)/sqrt(1-f^2)"
prop_D_of_arccos (Revertible1 f) = prop' [f::S10] "D(arccos(f)) == -D(f)/sqrt(1-f^2)"
prop_D_of_arctan (Revertible1 f) = prop' [f::S10] "D(arctan(f)) == D(f)/(1+f^2)"
prop_D_of_sinh   (Revertible1 f) = prop' [f::S10] "D(sinh(f))   == D(f)*cosh(f)"
prop_D_of_cosh   (Revertible1 f) = prop' [f::S10] "D(cosh(f))   == D(f)*sinh(f)"
prop_D_of_tanh   (Revertible1 f) = prop' [f::S10] "D(tanh(f))   == D(f)*(1-tanh(f)^2)"
prop_D_of_arsinh (Revertible1 f) = prop' [f::S10] "D(arsinh(f)) == D(f)/sqrt(1+f^2)"
prop_D_of_arcosh (Revertible1 f) = prop' [f::S10] "D(arcosh(f)) == D(f)/sqrt(f^2-1)"
prop_D_of_artanh (Revertible1 f) = prop' [f::S10] "D(artanh(f)) == D(f)/(1-f^2)"
prop_Hyperbolic_unit (FullRevertible1 f) = prop' [f::S10] "cosh(f)^2 - sinh(f)^2 == 1"

prop_Labeled_trees_1  = prop1 "A000272 == {1,1,n^(n-2)}"
prop_Labeled_trees_2  = prop1 "A000272 == T=x*exp(T);F=1+T-(1/2)*T^2;F.*{n!}"
prop_Unlabeled_trees  = prop1 "f=A000081;1+f-f^2/2+f(x^2)/2 == A000055"
prop_Labeled_graphs   = propN1 16 "A006125 == {2^(n*(n-1)/2)}"
prop_Fredholm_Rueppel = prop1 "A036987 == F=1+x*F(x^2)"

prop_Bell_1      = prop1  "A000110 == {1/n!}@{0,1/n!}.*{n!}"
prop_Bell_2      = prop1  "A000110 == e=1+integral(e);e(e-1).*{n!}"
prop_Bell_3      = prop1  "A000110 == y=x/(1-x);B=1+y*B(y)"
prop_Bell_4      = prop1  "A000110 == Bell=1+integral(exp(x)*Bell); Bell .* {n!}"
prop_Bessel      = prop1  "A006789 == B=1/(1-x-x^2*B(x/(1-x))/(1-x))"
prop_A000670_1   = prop1  "A000670 == 1/(2-exp(x)).*{n!}"
prop_A000670_2   = prop1  "A000670 == y=1+integral(2*y^2-y);y.*{n!}"
prop_A000670_3   = prop1  "A000670 == 1 + x*STIRLING({(n+1)!})"
prop_A000670_4   = prop1  "A000670 == A=integral(1+3*A+2*A^2);(1+A).*{n!}"
prop_Catalan_1   = prop1  "A000108 == {(2*n)!/(n!*(n+1)!)}"
prop_Catalan_2   = prop1' "A000108 == (1-sqrt(1-4*x))/(2*x)"
prop_Catalan_3   = propN1 10 "A000108 == 1/(1-x/(1-x/(1-x/(1-x/(1-x/(1-x/(1-x/(1-x/(1-x)))))))))"
prop_Catalan_4   = prop1 "A000108 == C=1+x*C^2"
prop_Motzkin_1   = propN1 (-2) "A001006 == (1-x-(1-2*x-3*x^2)^(1/2))/(2*x^2)"
prop_Motzkin_2   = prop1' "f=A000108;(f(x/(1+x))-1)/x == A001006"
prop_Motzkin_3   = propN1 11 "A001006 == 1/(1-x-x^2/(1-x-x^2/(1-x-x^2/(1-x-x^2/(1-x-x^2)))))"
prop_Motzkin_4   = prop1  "A001006 == M=1+x*M+x^2*M^2"
prop_Bous_1      = prop1  "A000111 == BOUS([1])"
prop_Bous_2      = prop1  "A000667 == -BOUS(-1/(1-x))"
prop_Bous_3      = prop1  "A062162 == BOUS(1/(1+x))"
prop_Euler       = prop1  "A122045 == 2/({1/n!}+{(-1)^n/n!}).*{n!}"
prop_Fibonacci_1 = prop1' "LEFT(A000045) == 1/(1-x-x^2)"
prop_Fibonacci_2 = prop1' "LEFT(A000045) == F=1+(x+x^2)*F"
prop_A001710_1   = prop1  "A001710 == (2-x^2)/(2-2*x).*{n!}"
prop_A001710_2   = prop1  "A001710 == {1,1,n!/2}"
prop_A235802     = prop1  "A235802 == 1/(1-x)^(2/(2-x)).*{n!}"
prop_A075834     = prop1  "A075834 == A=1+x/(1-x*D(A)/A)"
prop_A003149     = prop1' "A003149 == (log(1-x)/(x/2-1).*{n!})/x"
prop_A012259_1   = prop1  "A012259 == exp(artanh(tan(x))).*{n!}"
prop_A012259_2   = prop1  "A012259 == sqrt(sec(2*x)+tan(2*x)).*{n!}"
prop_A012259_3   = prop1  "A012259 == sqrt((1+tan(x))/(1-tan(x))).*{n!}"
prop_A202152     = prop1  "A202152 == exp(x*(1+x)^x).*{n!}"
prop_A191422     = prop1  "A191422 == (1+x+x^2)^x.*{n!}"
prop_A088789     = prop1  "A088789 == F=2*x/(1+exp(x));laplace(revert(F))"
prop_A049140     = prop1' "A049140 == LEFT(revert(x*(1-x-x^3)))"
prop_A008965     = prop1' "A008965 == CYC(I({n+1}))/x"

-- Naive cofactor implementation of determinant
determinant :: Num a => Vector (Vector a) -> a
determinant m =
  let n = V.length m
      minor i j = del j . V.map (del i)
      del k u = let (s, t) = V.splitAt k u in s <> V.tail t
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

prop_SEQ (Revertible f) g = prop [f,g::Series 30] "SEQ(f) == I(g)@f + IC(g)@f"

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

prop_MSET (Revertible f) = uncurry (~=) $ evalEqn [f::S10, mset f] "MSET(f) == g"
prop_PSET (Revertible f) = uncurry (~=) $ evalEqn [f::S10, pset f] "PSET(f) == g"

tests =
    [ ("Prg-monoid/id-1",        check 100 prop_Prg_id1)
    , ("Prg-monoid/id-2",        check 100 prop_Prg_id2)
    , ("Prg-monoid/associative", check 100 prop_Prg_assoc)
    , ("Prg-monoid/value",       check 100 prop_Prg_value)
    , ("unit/Rat-Power",         check   1 prop_Rat_power_u)
    , ("unit/Neg-Power",         check   1 prop_Neg_power_u)
    , ("unit/LEFT",              check   1 prop_LEFT_u)
    , ("unit/RIGHT",             check   1 prop_RIGHT_u)
    , ("unit/BINOMIAL",          check   1 prop_BINOMIAL_u)
    , ("unit/BINOMIALi",         check   1 prop_BINOMIALi_u)
    , ("unit/BIN1",              check   1 prop_BIN1_u)
    , ("unit/BISECT0",           check   1 prop_BISECT0_u)
    , ("unit/BISECT1",           check   1 prop_BISECT1_u)
    , ("unit/CATALAN",           check   1 prop_CATALAN_u)
    , ("unit/CATALANi",          check   1 prop_CATALANi_u)
    , ("unit/CYC",               check   1 prop_CYC_u)
    , ("unit/DIFF",              check   1 prop_DIFF_u)
    , ("unit/MOBIUS",            check   1 prop_MOBIUS_u)
    , ("unit/MOBIUSi",           check   1 prop_MOBIUSi_u)
    , ("unit/DIRICHLETi",        check   1 prop_DIRICHLETi_u)
    , ("unit/BOUS",              check   1 prop_BOUS_u)
    , ("unit/BOUSi",             check   1 prop_BOUSi_u)
    , ("unit/EULER",             check   1 prop_EULER_u)
    , ("unit/EULERi",            check   1 prop_EULERi_u)
    , ("unit/LAH",               check   1 prop_LAH_u)
    , ("unit/LAHi",              check   1 prop_LAHi_u)
    , ("unit/EXP",               check   1 prop_EXP_u)
    , ("unit/LOG",               check   1 prop_LOG_u)
    , ("unit/MSET",              check   1 prop_MSET_u)
    , ("unit/PSET",              check   1 prop_PSET_u)
    , ("unit/PRODS",             check   1 prop_PRODS_u)
    , ("unit/STIRLING",          check   1 prop_STIRLING_u)
    , ("unit/STIRLINGi",         check   1 prop_STIRLINGi_u)
    , ("unit/TRISECT0",          check   1 prop_TRISECT0_u)
    , ("unit/TRISECT1",          check   1 prop_TRISECT1_u)
    , ("unit/TRISECT2",          check   1 prop_TRISECT2_u)
    , ("unit/POINT",             check   1 prop_POINT_u)
    , ("unit/WEIGHT",            check   1 prop_WEIGHT_u)
    , ("unit/PARTITION",         check   1 prop_PARTITION_u)
    , ("unit/HANKEL",            check   1 prop_HANKEL_u)
    , ("unit/lHANKEL",           check   1 prop_lHANKEL_u)
    , ("unit/I",                 check   1 prop_I_u)
    , ("unit/IC",                check   1 prop_IC_u)
    , ("LEFT",                   check 100 prop_LEFT)
    , ("RIGHT",                  check 100 prop_RIGHT)
    , ("BINOMIALi.BINOMIAL=id",  check  40 prop_BINOMIALi_BINOMIAL)
    , ("BINOMIAL.BINOMIALi=id",  check  40 prop_BINOMIAL_BINOMIALi)
    , ("BOUSi.BOUS=id",          check  20 prop_BOUSi_BOUS)
    , ("BOUS.BOUSi=id",          check  20 prop_BOUS_BOUSi)
    , ("CATALAN.CATALANi=id",    check  20 prop_CATALAN_CATALANi)
    , ("CATALANi.CATALAN=id",    check  20 prop_CATALANi_CATALAN)
    , ("LAHi.LAH=id",            check   5 prop_LAHi_LAH)
    , ("LAH.LAHi=id",            check   5 prop_LAH_LAHi)
    , ("EULERi.EULER=id",        check   5 prop_EULERi_EULER)
    , ("EULER.EULERi=id",        check   5 prop_EULER_EULERi)
    , ("LOG.EXP=id",             check 100 prop_LOG_EXP)
    , ("EXP.LOG=id",             check 100 prop_EXP_LOG)
    , ("MOBIUSi_MOBIUS=id",      check 100 prop_MOBIUSi_MOBIUS)
    , ("MOBIUS.MOBIUSi=id",      check 100 prop_MOBIUS_MOBIUSi)
    , ("DIRICHLET.DIRICHLETi1=1",check 100 prop_DIRICHLET_DIRICHLETi1)
    , ("DIRICHLET.DIRICHLETi2=1",check 100 prop_DIRICHLET_DIRICHLETi2)
    , ("STIRLINGi_STIRLING=id",  check 100 prop_STIRLINGi_STIRLING)
    , ("STIRLING.STIRLINGi=id",  check 100 prop_STIRLING_STIRLINGi)
    , ("BIN1/involutive",        check 100 prop_BIN1_involutive)
    , ("BINOMIAL = expr",        check  50 prop_BINOMIAL)
    , ("BINOMIALi = expr",       check  50 prop_BINOMIALi)
    , ("BIN1 = expr",            check 100 prop_BIN1)
    , ("DIFF = expr",            check 100 prop_DIFF)
    , ("LAH  = (f./{n!})@(x/(1-x)).*{n!}", check 100 prop_LAH)
    , ("LAHi = (f./{n!})@(x/(1+x)).*{n!}", check 100 prop_LAHi)
    , ("EXP = expr",             check 100 prop_EXP)
    , ("LOG = expr",             check 100 prop_LOG)
    , ("STIRLING = expr",        check 100 prop_STIRLING)
    , ("STIRLINGi = expr",       check 100 prop_STIRLINGi)
    , ("POINT = x*D(f./{n!}).*{n!}", check 100 prop_POINT)
    , ("distributive1",          check 100 prop_Distrib1)
    , ("distributive2",          check 100 prop_Distrib2)
    , ("distributive3",          check 100 prop_Distrib3)
    , ("reflexivity",            check 100 prop_Reflexivity)
    , ("eval: [2^n]",            check 100 prop_Powers_of_2)
    , ("eval: x@f=f",            check 100 prop_Compose1)
    , ("eval: f@x=f",            check 100 prop_Compose2)
    , ("Bell-1",                 check   1 prop_Bell_1)
    , ("Bell-2",                 check   1 prop_Bell_2)
    , ("Bell-3",                 check   1 prop_Bell_3)
    , ("Bell-4",                 check   1 prop_Bell_4)
    , ("Bessel",                 check   1 prop_Bessel)
    , ("Catalan-1",              check   1 prop_Catalan_1)
    , ("Catalan-2",              check   1 prop_Catalan_2)
    , ("Catalan-3",              check   1 prop_Catalan_3)
    , ("Catalan-4",              check   1 prop_Catalan_4)
    , ("Motzkin-1",              check   1 prop_Motzkin_1)
    , ("Motzkin-2",              check   1 prop_Motzkin_2)
    , ("Motzkin-3",              check   1 prop_Motzkin_3)
    , ("Motzkin-4",              check   1 prop_Motzkin_4)
    , ("Boustrophedon-1",        check   1 prop_Bous_1)
    , ("Boustrophedon-2",        check   1 prop_Bous_2)
    , ("Boustrophedon-3",        check   1 prop_Bous_3)
    , ("Euler",                  check   1 prop_Euler)
    , ("Fibonacci-1",            check   1 prop_Fibonacci_1)
    , ("Fibonacci-2",            check   1 prop_Fibonacci_2)
    , ("List of coeffs",         check 100 prop_ListOfCoeffs)
    , ("Polynomial",             check  50 prop_Polynomial)
    , ("Arithmetic progression", check 100 prop_AP)
    , ("Geometric series 1",     check 100 prop_Geometric1)
    , ("Geometric series 2",     check 100 prop_Geometric2)
    , ("Labeled trees 1",        check   1 prop_Labeled_trees_1)
    , ("Labeled trees 2",        check   1 prop_Labeled_trees_2)
    , ("Unlabeled trees",        check   1 prop_Unlabeled_trees)
    , ("Labeled graphs",         check   1 prop_Labeled_graphs)
    , ("Connected labeled graphs", check 1 prop_Connected_labeled_graphs)
    , ("Derivative of geometric",  check 1 prop_D_of_geometric)
    , ("Product rule",           check  50 prop_Product_rule)
    , ("Reciprocal rule",        check  50 prop_Reciprocal_rule)
    , ("Chain rule",             check  50 prop_Chain_rule)
    , ("Fund Thm of Calc 1",     check 100 prop_Fundamental1)
    , ("Fund Thm of Calc 2",     check 100 prop_Fundamental2)
    , ("Integration by parts",   check 100 prop_Integration_by_parts)
    , ("A235802",                check   1 prop_A235802)
    , ("A001710-1",              check   1 prop_A001710_1)
    , ("A001710-2",              check   1 prop_A001710_2)
    , ("A075834",                check   1 prop_A075834)
    , ("A003149",                check   1 prop_A003149)
    , ("Fredholm-Rueppel",       check   1 prop_Fredholm_Rueppel)
    , ("A000670-1",              check   1 prop_A000670_1)
    , ("A000670-2",              check   1 prop_A000670_2)
    , ("A000670-3",              check   1 prop_A000670_3)
    , ("A000670-4",              check   1 prop_A000670_4)
    , ("Increasing forests",     check   1 prop_IncreasingForests)
    , ("Unit circle",            check  20 prop_Unit_circle)
    , ("Exact sqrt",             check  50 prop_Exact_sqrt)
    , ("Derivative of sin",      check  50 prop_D_of_sin)
    , ("Derivative of cos",      check  50 prop_D_of_cos)
    , ("Derivative of tan",      check  50 prop_D_of_tan)
    , ("Derivative of arcsin",   check  50 prop_D_of_arcsin)
    , ("Derivative of arccos",   check  50 prop_D_of_arccos)
    , ("Derivative of arctan",   check  50 prop_D_of_arctan)
    , ("Derivative of sinh",     check  50 prop_D_of_sinh)
    , ("Derivative of cosh",     check  50 prop_D_of_cosh)
    , ("Derivative of tanh",     check  50 prop_D_of_tanh)
    , ("Derivative of arsinh",   check   5 prop_D_of_arsinh)
    , ("Derivative of arcosh",   check   5 prop_D_of_arcosh)
    , ("Derivative of artanh",   check  50 prop_D_of_artanh)
    , ("sinh",                   check  50 prop_sinh)
    , ("cosh",                   check  50 prop_cosh)
    , ("tanh",                   check  50 prop_tanh)
    , ("Hyperbolic unit",        check  50 prop_Hyperbolic_unit)
    , ("arsinh",                 check  50 prop_arsinh)
    , ("artanh",                 check  50 prop_artanh)
    , ("A012259-1",              check   1 prop_A012259_1)
    , ("A012259-2",              check   1 prop_A012259_2)
    , ("A012259-3",              check   1 prop_A012259_3)
    , ("A202152",                check   1 prop_A202152)
    , ("A191422",                check   1 prop_A191422)
    , ("A088789",                check   1 prop_A088789)
    , ("A049140",                check   1 prop_A049140)
    , ("A008965",                check   1 prop_A008965)
    , ("Determinant",            check 100 prop_Determinant)
    , ("Coefficients",           check   1 prop_Coefficients)
    , ("SEQ identity",           check  50 prop_SEQ)
    , ("MSET identity",          check  50 prop_MSET)
    , ("PSET identity",          check  50 prop_PSET)
    ]

main =
    forM_ tests $ \(name, chk) -> do
        putStr (name ++ ": ")
        result <- chk
        unless (isSuccess result) exitFailure
