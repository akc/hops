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
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Control.Applicative
import Control.Monad
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test
import Test.QuickCheck.Modifiers
import HOPS.Matrix
import HOPS.GF
import qualified HOPS.GF.Const as C
import qualified HOPS.GF.Rats as R
import HOPS.GF.Series
import HOPS.GF.Transform

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

instance KnownNat n => Arbitrary (NonNil n) where
    arbitrary = NonNil <$> arbitrary `suchThat` nonzeroLeadingVal

instance KnownNat n => Arbitrary (NonDZ n) where
    arbitrary = NonDZ . series (Proxy :: Proxy n) <$> arbitrary `suchThat` (all (/= DZ))

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
        , ( 1, Tr     <$> nameGen <*> arbitrary)
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

runPrg :: KnownNat n => Env n -> ByteString -> Series n
runPrg env = evalPrg env . fromMaybe (error "parse error") . parsePrg

runPrg1 :: KnownNat n => ByteString -> Series n
runPrg1 = runPrg (Env V.empty M.empty)

ogf :: KnownNat n => Proxy n -> [Integer] -> Series n
ogf n = series n . map (Val . fromIntegral)

poly :: KnownNat n => Proxy n -> [Integer] -> Series n
poly n = polynomial n . map (Val . fromIntegral)

ogf20 = ogf (Proxy :: Proxy 20)
poly20 = poly (Proxy :: Proxy 20)
empty20 = Env V.empty M.empty :: Env 20
runPrg20 = runPrg empty20

ogf40 = ogf (Proxy :: Proxy 40)
poly40 = poly (Proxy :: Proxy 40)
empty40 = Env V.empty M.empty :: Env 40
runPrg40 = runPrg empty40

envFromList :: KnownNat n => [(ByteString, Series n)] -> Env n
envFromList assoc = Env V.empty (M.fromList assoc)

stdinTr :: KnownNat n => ByteString -> Series n -> Series n
stdinTr prg f = runPrg (envFromList [("stdin", f)]) prg

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
    exec = runPrg $ envFromList (zip nameSupply fs)

areEq :: KnownNat n => [Series n] -> ByteString -> Bool
areEq fs = uncurry (==) . evalEqn fs

areEqN :: KnownNat n => Int -> [Series n] -> ByteString -> Bool
areEqN m fs = uncurry (contactOfOrder m) . evalEqn fs

areSimEq :: KnownNat n => [Series n] -> ByteString -> Bool
areSimEq fs = uncurry (~=) . evalEqn fs

areEq' :: KnownNat n => [Series n] -> ByteString -> Bool
areEq' = areEqN (-1)

areEq0 :: ByteString -> Bool
areEq0 = areEq [nil::Series 13]

areEq0' :: ByteString -> Bool
areEq0' = areEq' [nil::Series 13]

toRatsString :: Show a => [a] -> String
toRatsString xs = "{" ++ intercalate "," (map show xs) ++ "}"

prop_Prg_id1 p = mempty <> p == (p :: Prg)
prop_Prg_id2 p = p <> mempty == (p :: Prg)
prop_Prg_assoc p q r = p <> (q <> r) == (p <> q) <> (r :: Prg)

prop_Prg_value p = evalPrg' p == evalPrg' q
  where
    evalPrg' = evalPrg empty40
    q = p <> fromJust (parsePrg "stdin") :: Prg

prop_Rat_power_u :: Bool
prop_Rat_power_u = (1/4) !^! (3/2) == Val (1 % 8)

prop_Neg_power_u = areEq0 "{-(-1)^n} == -1/(1+x)"

prop_LEFT_u      = areEq0 "LEFT      {4,3,2,1}          == {3,2,1}"
prop_RIGHT_u     = areEq0 "RIGHT     {4,3,2,1}          == {1,4,3,2,1}"
prop_BINOMIAL_u  = areEq0 "BINOMIAL  {1,2,4,8,16}       == {1,3,9,27,81}"
prop_BINOMIALi_u = areEq0 "BINOMIALi {1,3,9,27,81}      == {1,2,4,8,16}"
prop_BIN1_u      = areEq0 "BIN1      {2,4,8,16}         == {2,-8,26,-80}"
prop_BISECT0_u   = areEq0 "BISECT0   {0,1,2,3,4,5}      == {0,2,4}"
prop_BISECT1_u   = areEq0 "BISECT1   {0,1,2,3,4,5}      == {1,3,5}"
prop_BOUS_u      = areEq0 "BOUS      {5,4,3,2,1}        == {1,6,15,32,83,262}"
prop_BOUS2_u     = areEq0 "BOUS2     {5,4,3,2,1}        == {5,9,16,33,84}"
prop_BOUS2i_u    = areEq0 "BOUS2i    {5,4,3,2,1}        == {5,-1,0,-5,4}"
prop_CATALAN_u   = areEq0 "CATALAN   {1,1,1,1,1}        == {1,1,2,5,14}"
prop_CATALANi_u  = areEq0 "CATALANi  {1,1,2,5,14}       == {1,1,1,1,1}"
prop_CYC_u       = areEq0 "CYC       {0,1,1,1,1,1}      == {0,1,2,3,5,7}"
prop_DIFF_u      = areEq0 "DIFF      {9,4,1,0,1,4,9}    == {-5,-3,-1,1,3,5}"
prop_MOBIUS_u    = areEq0 "MOBIUS    {1,3,4,7,6,12}     == {1,2,3,4,5,6}"
prop_MOBIUSi_u   = areEq0 "MOBIUSi   {1,2,3,4,5,6}      == {1,3,4,7,6,12}"
prop_EULER_u     = areEq0 "EULER     {1,1,0,0,0,0,0}    == {1,2,2,3,3,4,4}"
prop_EULERi_u    = areEq0 "EULERi    {1,2,2,3,3,4,4}    == {1,1,0,0,0,0,0}"
prop_LAH_u       = areEq0 "LAH       {5,4,3,2,1}        == {5,4,11,44,229}"
prop_LAHi_u      = areEq0 "LAHi      {5,4,3,2,1}        == {5,4,-5,8,-11}"
prop_EXP_u       = areEq0 "EXP       {1,2,3,4}          == {1,3,10,41}"
prop_LOG_u       = areEq0 "LOG       {1,3,10,41}        == {1,2,3,4}"
prop_MSET_u      = areEq0 "MSET      {0,1,0,1}          == {1,1,1,2}"
prop_PRODS_u     = areEq0 "PRODS     {1,2,3,4,5}        == {1,2,6,24,120}"
prop_PSET_u      = areEq0 "PSET      {0,2,1}            == {1,2,2}"
prop_SEQ_u       = areEq0 "SEQ       {0,1,1,0,0,0}      == {1,1,2,3,5,8}"
prop_STIRLING_u  = areEq0 "STIRLING  {1,2,3,4,5}        == {1,3,10,37,151}"
prop_STIRLINGi_u = areEq0 "STIRLINGi {1,3,10,37,151}    == {1,2,3,4,5}"
prop_TRISECT0_u  = areEq0 "TRISECT0  {0,1,2,3,4,5,6}    == {0,3,6}"
prop_TRISECT1_u  = areEq0 "TRISECT1  {0,1,2,3,4,5,6}    == {1,4}"
prop_TRISECT2_u  = areEq0 "TRISECT2  {0,1,2,3,4,5,6}    == {2,5}"
prop_POINT_u     = areEq0 "POINT     {1,1,4,27,256}     == {0,1,8,81,1024}"
prop_WEIGHT_u    = areEq0 "WEIGHT    {1,1,1,1,1,1,1,1}  == {1,1,2,2,3,4,5,6}"
prop_PARTITION_u = areEq0 "PARTITION {1,3,5} == [1,1,1,2,2,3,4,4,5,6,7,8,9,10,11,13,14,15,17,18]"
prop_HANKEL_u    = areEq0 "HANKEL    {6,5,4,3,2,1}      == {6,-1,0,0}"
prop_lHANKEL_u   = areEq0 "lHANKEL   {1,4,9,16,25,36}   == {7,17,31,49}"
prop_I_u         = areEq0 "I         {2,4}              == [0,0,1,0,1]"
prop_IC_u        = areEq0 "IC {2,4} == [1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]"

prop_LEFT                     f  = areEq  [f::Series 20] "LEFT(f) == D(f./{n!}) .* {n!}"
prop_RIGHT             (NonDZ f) = areEq  [f::Series 20] "RIGHT(f) == 1 + x*f"
prop_BINOMIALi_BINOMIAL (Full f) = areEq  [f::Series 20] "f == BINOMIALi(BINOMIAL(f))"
prop_BINOMIAL_BINOMIALi (Full f) = areEq  [f::Series 20] "f == BINOMIAL(BINOMIALi(f))"
prop_BOUS2i_BOUS2       (Full f) = areEq  [f::Series 20] "f == BOUS2i(BOUS2(f))"
prop_BOUS2_BOUS2i       (Full f) = areEq  [f::Series 20] "f == BOUS2(BOUS2i(f))"
prop_CATALANi_CATALAN   (Full f) = areEq  [f::Series 20] "f == CATALANi(CATALAN(f))"
prop_CATALAN_CATALANi   (Full f) = areEq  [f::Series 20] "f == CATALAN(CATALANi(f))"
prop_LAHi_LAH           (Full f) = areEq  [f::Series  9] "f == LAHi(LAH(f))"
prop_LAH_LAHi           (Full f) = areEq  [f::Series  9] "f == LAH(LAHi(f))"
prop_EULERi_EULER       (Full f) = areEq' [f::Series  6] "f == EULERi(EULER(f))"
prop_EULER_EULERi       (Full f) = areEq' [f::Series  6] "f == EULER(EULERi(f))"
prop_LOG_EXP            (Full f) = areEq' [f::Series 15] "f == LOG(EXP(f))"
prop_EXP_LOG            (Full f) = areEq' [f::Series 20] "f == EXP(LOG(f))"
prop_MOBIUSi_MOBIUS     (Full f) = areEq  [f::Series 20] "f == MOBIUSi(MOBIUS(f))"
prop_MOBIUS_MOBIUSi     (Full f) = areEq  [f::Series 20] "f == MOBIUS(MOBIUSi(f))"
prop_STIRLINGi_STIRLING (Full f) = areEq' [f::Series 20] "f == STIRLINGi(STIRLING(f))"
prop_STIRLING_STIRLINGi (Full f) = areEq' [f::Series 10] "f == STIRLING(STIRLINGi(f))"
prop_BIN1_involutive    (Full f) = areEq' [f::Series  8] "f == BIN1(BIN1(f))"
prop_Compose1           (Full f) = areEq  [f::Series 20] "f == x@f"
prop_Compose2           (Full f) = areEq  [f::Series 20] "f == f@x"

prop_BINOMIAL  f = areEq  [f::Series 20] "BINOMIAL(f)  == (f ./ {n!}) * {1/n!} .* {n!}"
prop_BINOMIALi f = areEq  [f::Series 20] "BINOMIALi(f) == (f ./ {n!}) * {(-1)^n/n!} .* {n!}"
prop_BIN1      f = areEq  [f::Series 20] "BIN1(f)      == LEFT((-{(-1)^n/n!} * (((x*f) ./ {n!})@(-x))) .* {n!})"
prop_DIFF      f = areEq' [f::Series 20] "DIFF(f)      == (D(f./{n!}) .* {n!} - f)"
prop_LAH       f = areEq  [f::Series 20] "LAH(f)       == (f./{n!})@(x/(1-x)) .* {n!}"
prop_LAHi      f = areEq  [f::Series 20] "LAHi(f)      == (f./{n!})@(x/(1+x)) .* {n!}"
prop_EXP       f = areEq' [f::Series 20] "EXP(f)       == (({1/n!}@(x*f./{n!}) - 1) .* {n!})/x"
prop_LOG       f = areEq' [f::Series 20] "LOG(f)       == ({0,(-1)^(n+1)/n}@(x*f./{n!}) .* {n!})/x"
prop_STIRLING  f = areEq' [f::Series 20] "STIRLING(f)  == ((x*f ./ {n!})@({0,1/n!}) .* {n!})/x"
prop_STIRLINGi f = areEq' [f::Series 20] "STIRLINGi(f) == ((x*f ./ {n!})@({0,(-1)^(n+1)/n}) .* {n!})/x"
prop_POINT     f = areEq  [f::Series 20] "POINT(f)     == x*D(f./{n!}) .* {n!}"

prop_Distrib1 :: Series 20 -> Series 20 -> Series 20 -> Bool
prop_Distrib1 f g h = f*(g+h) ~== f*g + f*h

prop_Distrib2 :: Series 20 -> Series 20 -> Series 20 -> Bool
prop_Distrib2 f g h = f.*(g+h) ~== (f.*g + f.*h)

prop_Distrib3 :: Series 20 -> Series 20 -> Series 20 -> Bool
prop_Distrib3 f g h = g./f + h./f ~== (g+h)./f

prop_Reflexivity f = areEq [f::Series 1000] "f == f"

prop_Powers_of_2 f = areEq [f::Series 30]"{2^n} == 1/(1-2*x)"

dropTrailingZeros = reverse . dropWhile (== 0) . reverse

prop_ListOfCoeffs f = runPrg1 (pretty f) == (f :: Series 2)

prop_Polynomial (Full f) = runPrg1 p == (f :: Series 20)
  where
    cs = coeffList f
    term c i = B.concat [pretty c, "*x^", pretty i]
    p = B.intercalate "+" (zipWith term cs [0::Integer ..])

-- Arithmetic progression
prop_AP a b = ogf20 [a,b..] == runPrg20 (B.concat ["{", pretty a, ",", pretty b, ",...}"])

prop_Geometric1 c = areEq [ogf20 [c^i | i<-[0..]]] (B.pack $ printf "f == {(%s)^n}" (show c))
prop_Geometric2 c = areEq [ogf20 [c^i | i<-[0..]]] (B.pack $ printf "f == 1/(1 - %s*x)" (show c))

prop_Connected_labeled_graphs =
    areEq [inp, ans] "1 + {0,(-1)^(n+1)/n}@(f./{n!}) .* {n!} == g"
  where
    inp = ogf20 [0,1,2,8,64,1024,32768,2097152,268435456,68719476736,35184372088832]
    -- A001187: Number of connected labeled graphs with n nodes
    ans = ogf20 [1,1,1,4,38,728,26704,1866256,251548592,66296291072,34496488594816]

prop_Derivative_of_geometric = areEq0' "D(1/(1-x)) == (1/(1-x))^2"

prop_Product_rule (Full1 f) (Full1 g) = areEq' [f,g::Series 3]  "D(f*g) == D(f)*g + f*D(g)"
prop_Reciprocal_rule (FullUnit f)     = areEq  [f::Series 20]   "D(1/f) == -D(f)/f^2"
prop_Chain_rule (Revertible f) g      = areEq' [f,g::Series 20] "D(g@f) == (D(g))@f * D(f)"

-- Fundametal Theorem of Calculus
prop_Fundamental1 f = areEq' [f::Series 20] "f == f(0) + integral(D(f))"
prop_Fundamental2 f = areEq' [f::Series 20] "f == D(integral(f))"

-- Integration by parts
prop_Integration_by_parts f g =
    areEq [f,g::Series 20] "f*g == (f*g)@0 + integral(D(f)*g) + integral(f*D(g))"

prop_IncreasingForests = areEq0' "tree=integral(forest);forest=exp(tree) == 1/(1-x)"

prop_Unit_circle (FullRevertible1 f) = areEq' [f::Series 15] "cos(f)^2 + sin(f)^2 == 1"

prop_Exact_sqrt c d =
    let prg = B.pack ("sqrt({(" ++ show c ++ ")^2/(" ++ show d ++ ")^2})")
    in runPrg empty20 prg ~= ogf20 [abs c::Integer] / ogf20 [abs d::Integer]

prop_Derivative_of_sin    (Revertible f)  = areEq' [f::Series 20] "D(sin(f))    == D(f)*cos(f)"
prop_Derivative_of_cos    (Revertible f)  = areEq' [f::Series 20] "D(cos(f))    == -D(f)*sin(f)"
prop_Derivative_of_tan    (Revertible f)  = areEq' [f::Series 20] "D(tan(f))    == D(f)/cos(f)^2"
prop_Derivative_of_arcsin (Revertible f)  = areEq' [f::Series 20] "D(arcsin(f)) == D(f)/sqrt(1-f^2)"
prop_Derivative_of_arccos (Revertible1 f) = areEq' [f::Series 20] "D(arccos(f)) == -D(f)/sqrt(1-f^2)"
prop_Derivative_of_arctan (Revertible1 f) = areEq' [f::Series 20] "D(arctan(f)) == D(f)/(1+f^2)"
prop_Derivative_of_sinh   (Revertible1 f) = areEq' [f::Series 20] "D(sinh(f))   == D(f)*cosh(f)"
prop_Derivative_of_cosh   (Revertible1 f) = areEq' [f::Series 20] "D(cosh(f))   == D(f)*sinh(f)"
prop_Derivative_of_tanh   (Revertible1 f) = areEq' [f::Series 20] "D(tanh(f))   == D(f)*(1-tanh(f)^2)"
prop_Derivative_of_arsinh (Revertible1 f) = areEq' [f::Series 20] "D(arsinh(f)) == D(f)/sqrt(1+f^2)"
prop_Derivative_of_arcosh (Revertible1 f) = areEq' [f::Series 20] "D(arcosh(f)) == D(f)/sqrt(f^2-1)"
prop_Derivative_of_artanh (Revertible1 f) = areEq' [f::Series 20] "D(artanh(f)) == D(f)/(1-f^2)"
prop_Hyperbolic_unit  (FullRevertible1 f) = areEq' [f::Series 10] "cosh(f)^2 - sinh(f)^2 == 1"

prop_sinh     (Revertible1 f) = areEq' [f::Series 20] "sinh(f)   == (exp(f)-exp(-f))/2"
prop_cosh (FullRevertible1 f) = areEq' [f::Series 20] "cosh(f)   == (exp(f)+exp(-f))/2"
prop_tanh     (Revertible1 f) = areEq' [f::Series 20] "tanh(f)   == (exp(f)-exp(-f))/(exp(f)+exp(-f))"
prop_arsinh   (Revertible1 f) = areEq' [f::Series 10] "arsinh(f) == log(f + sqrt(f^2 + 1))"
prop_artanh   (Revertible1 f) = areEq' [f::Series 10] "artanh(f) == (log(1+f) - log(1-f))/2"

a000272 = ogf (Proxy :: Proxy 19) -- Number of trees on n labeled nodes
    [ 1,1,1,3,16,125,1296,16807,262144,4782969,100000000,2357947691
    , 61917364224,1792160394037,56693912375296,1946195068359375
    , 72057594037927936,2862423051509815793,121439531096594251776
    ]
a000081 = ogf (Proxy :: Proxy 31)
    [ 0,1,1,2,4,9,20,48,115,286,719,1842,4766,12486,32973,87811,235381,634847
    , 1721159,4688676,12826228,35221832,97055181,268282855,743724984,2067174645
    , 5759636510,16083734329,45007066269,126186554308,354426847597
    ]
a000055 = ogf (Proxy :: Proxy 31)
    [ 1,1,1,1,2,3,6,11,23,47,106,235,551,1301,3159,7741,19320,48629,123867,317955
    , 823065,2144505,5623756,14828074,39299897,104636890,279793450,751065460
    , 2023443032,5469566585,14830871802
    ]
a006125 = ogf (Proxy :: Proxy 16)
    [ 1,1,2,8,64,1024,32768,2097152,268435456,68719476736,35184372088832
    , 36028797018963968,73786976294838206464,302231454903657293676544
    , 2475880078570760549798248448,40564819207303340847894502572032
    ]
a000110 = ogf (Proxy :: Proxy 27) -- Bell numbers
    [ 1,1,2,5,15,52,203,877,4140,21147,115975,678570,4213597,27644437,190899322
    , 1382958545,10480142147,82864869804,682076806159,5832742205057,51724158235372
    , 474869816156751,4506715738447323,44152005855084346,445958869294805289
    , 4638590332229999353,49631246523618756274
    ]
a006789 = ogf (Proxy :: Proxy 25) -- Bessel numbers
    [ 1,1,2,5,14,43,143,509,1922,7651,31965,139685,636712,3020203,14878176
    , 75982829,401654560,2194564531,12377765239,71980880885,431114329728
    , 2656559925883,16825918195484,109439943234749,730365368850192
    ]
a000670 = ogf (Proxy :: Proxy 21) -- Number of ballots / ordered set partitions
    [ 1,1,3,13,75,541,4683,47293,545835,7087261,102247563,1622632573,28091567595
    , 526858348381,10641342970443,230283190977853,5315654681981355
    , 130370767029135901,3385534663256845323,92801587319328411133
    , 2677687796244384203115
    ]
a000108 = ogf (Proxy :: Proxy 30) -- Catalan numbers
    [ 1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900,2674440,9694845
    , 35357670,129644790,477638700,1767263190,6564120420,24466267020,91482563640
    , 343059613650,1289904147324,4861946401452,18367353072152,69533550916004
    , 263747951750360,1002242216651368
    ]
a001006 = ogf (Proxy :: Proxy 30) -- Motzkin numbers
    [ 1,1,2,4,9,21,51,127,323,835,2188,5798,15511,41835,113634,310572,853467
    , 2356779,6536382,18199284,50852019,142547559,400763223,1129760415
    , 3192727797,9043402501,25669818476,73007772802,208023278209,593742784829
    ]
a122045 = ogf (Proxy :: Proxy 30) -- Euler (or secant) numbers
    [ 1,0,-1,0,5,0,-61,0,1385,0,-50521,0,2702765,0,-199360981,0,19391512145,0
    , -2404879675441,0,370371188237525,0,-69348874393137901,0,15514534163557086905
    , 0,-4087072509293123892361,0,1252259641403629865468285,0
    ]
fibonacci = ogf (Proxy :: Proxy 38)
    [ 1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946
    , 17711,28657,46368,75025,121393,196418,317811,514229,832040,1346269
    , 2178309,3524578,5702887,9227465,14930352,24157817,39088169
    ]
a235802 = ogf (Proxy :: Proxy 21)
    [ 1,1,3,12,61,375,2697,22176,204977,2102445,23685615,290642220,3857751573
    , 55063797243,840956549517,13682498891040,236257301424225,4314883836968505
    , 83102361300891963,1683252077760375660,35770269996769203405
    ]
a075834 = ogf (Proxy :: Proxy 21)
    [ 1,1,1,2,7,34,206,1476,12123,111866,1143554,12816572,156217782,2057246164
    , 29111150620,440565923336,7101696260883,121489909224618,2198572792193786
    , 41966290373704332,842706170872913634
    ]
a003149 = ogf (Proxy :: Proxy 21)
    [ 1,2,5,16,64,312,1812,12288,95616,840960,8254080,89441280,1060369920
    , 13649610240,189550368000,2824077312000,44927447040000,760034451456000
    , 13622700994560000,257872110354432000,5140559166898176000
    ]
a036987 = ogf (Proxy :: Proxy 60) -- Fredholm-Rueppel sequence
    [ 1,1,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    , 0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    ]
a012259 = ogf (Proxy :: Proxy 22)
    [ 1, 1, 1, 5, 17, 121, 721, 6845, 58337, 698161, 7734241, 111973685, 1526099057
    , 25947503401, 419784870961, 8200346492525, 153563504618177, 3389281372287841
    , 72104198836466881, 1774459993676715365, 42270463533824671697
    , 1147649139272698443481
    ]
a202152 = ogf (Proxy :: Proxy 24)
    [ 1,1,1,7,13,101,361,2269,18201,48817,1436401,-2283269,157443397,-826037939
    , 21355181849,-160556822999,3084325024561,-22223879489055,291212769688417
    , 2180748026158255,-118745486165378819,4884619264768661461,-140063412525642293687
    , 4020051993317128467029
    ]
a191422 = ogf (Proxy :: Proxy 21)
    [ 1,0,2,3,-4,90,-126,-840,21104,-137592,-88920,15741000,-197234808,535289040
    , 25582565904,-522317151720,3223601137920,75590725210560,-2388641226278976
    , 23718732310200960,361277667059425920
    ]
a088789 = ogf (Proxy :: Proxy 19)
    [ 0,1,1,3,14,90,738,7364,86608,1173240,17990600,308055528,5826331440
    , 120629547584,2713659864832,65909241461760,1718947213795328
    , 47912968352783232,1421417290991105664
    ]
a049140 = ogf (Proxy :: Proxy 19)
    [ 1,1,2,6,20,70,256,969,3762,14894,59904,244088,1005452,4180096
    , 17516936,73913705,313774854,1339162028,5742691704
    ]
a008965 = ogf (Proxy :: Proxy 19)
    [ 1,2,3,5,7,13,19,35,59,107,187,351,631,1181,2191,4115,7711,14601,27595
    ]
a001710 = ogf (Proxy :: Proxy 21) -- Order of the alternating group A_n
    [ 1,1,1,3,12,60,360,2520,20160,181440,1814400,19958400,239500800,3113510400
    , 43589145600,653837184000,10461394944000,177843714048000,3201186852864000
    , 60822550204416000,1216451004088320000
    ]

prop_Labeled_trees_1 = areEq [a000272] "f == {1,1,n^(n-2)}"
prop_Labeled_trees_2 = areEq [a000272] "f == T=x*exp(T);F=1+T-(1/2)*T^2;F.*{n!}"
prop_Unlabeled_trees = areEq [a000081, a000055] "1+f-f^2/2+f(x^2)/2 == g"
prop_Labeled_graphs  = areEq [a006125] "f == {2^(n*(n-1)/2)}"
prop_Bell_1      = areEq  [a000110] "f == {1/n!}@{0,1/n!}.*{n!}"
prop_Bell_2      = areEq  [a000110] "f == e=1+integral(e);e(e-1).*{n!}"
prop_Bell_3      = areEq  [a000110] "f == y=x/(1-x);B=1+y*B(y)"
prop_Bell_4      = areEq  [a000110] "f == Bell=1+integral(exp(x)*Bell); Bell .* {n!}"
prop_Bessel      = areEq  [a006789] "f == B=1/(1-x-x^2*B(x/(1-x))/(1-x))"
prop_A000670_1   = areEq  [a000670] "f == 1/(2-exp(x)).*{n!}"
prop_A000670_2   = areEq  [a000670] "f == y=1+integral(2*y^2-y);y.*{n!}"
prop_A000670_3   = areEq  [a000670] "f == 1 + x*STIRLING({(n+1)!})"
prop_A000670_4   = areEq  [a000670] "f == A=integral(1+3*A+2*A^2);(1+A).*{n!}"
prop_Catalan_1   = areEq  [a000108] "f == {(2*n)!/(n!*(n+1)!)}"
prop_Catalan_2   = areEq' [a000108] "f == (1-sqrt(1-4*x))/(2*x)"
prop_Catalan_3   = areEqN 12 [a000108] "f == 1/(1-x/(1-x/(1-x/(1-x/(1-x/(1-x/(1-x/(1-x/(1-x/(1-x/(1-x)))))))))))"
prop_Catalan_4   = areEq  [a000108] "f == C=1+x*C^2"
prop_Motzkin_1   = areEqN (-2) [a001006] "f == (1-x-(1-2*x-3*x^2)^(1/2))/(2*x^2)"
prop_Motzkin_2   = areEq' [a000108, a001006] "(f(x/(1+x))-1)/x == g"
prop_Motzkin_3   = areEqN 13 [a001006] "f == 1/(1-x-x^2/(1-x-x^2/(1-x-x^2/(1-x-x^2/(1-x-x^2/(1-x-x^2))))))"
prop_Motzkin_4   = areEq  [a001006] "f == M=1+x*M+x^2*M^2"
prop_Euler       = areEq  [a122045] "f == 2/({1/n!}+{(-1)^n/n!}).*{n!}"
prop_Fibonacci_1 = areEq  [fibonacci] "f == 1/(1-x-x^2)"
prop_Fibonacci_2 = areEq  [fibonacci] "f == F=1+(x+x^2)*F"
prop_A001710_1   = areEq  [a001710] "f == (2-x^2)/(2-2*x).*{n!}"
prop_A001710_2   = areEq  [a001710] "f == {1,1,n!/2}"
prop_A235802     = areEq  [a235802] "f == 1/(1-x)^(2/(2-x)).*{n!}"
prop_A075834_1   = areEq  [a075834] "f == A=1+x/(1-x*D(A)/A)"
prop_A075834_2   = areEq0 "A=1+x/(1-x*D(A)/A) == A=1+x/(1-(x*D(A))/A)"
prop_A003149     = areEq' [a003149] "f == (log(1-x)/(x/2-1).*{n!})/x"
prop_Fredholm_Rueppel = areEq [a036987] "f == F=1+x*F(x^2)"
prop_A012259_1   = areEq  [a012259] "f == exp(artanh(tan(x))).*{n!}"
prop_A012259_2   = areEq  [a012259] "f == sqrt(sec(2*x)+tan(2*x)).*{n!}"
prop_A012259_3   = areEq  [a012259] "f == sqrt((1+tan(x))/(1-tan(x))).*{n!}"
prop_A202152     = areEq  [a202152] "f == exp(x*(1+x)^x).*{n!}"
prop_A191422     = areEq  [a191422] "f == (1+x+x^2)^x.*{n!}"
prop_A088789     = areEq  [a088789] "f == F=2*x/(1+exp(x));laplace(revert(F))"
prop_A049140     = areEq' [a049140] "f == LEFT(revert(x*(1-x-x^3)))"
prop_A008965     = areEq' [a008965] "f == CYC(I({n+1}))/x"

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

prop_Coefficients = and
    [ areEq [f] "f == (1/(1-[0,1,1]))?[2*n+1]"
    , areEq [f] "f == (1/(1-[0,1,1]))?{2*n+1}"
    , areEq [a] "f == (1/(1-[0,1,1]))?5"
    ]
  where
    f = ogf20 [1,3,8,21,55,144,377,987,2584,6765]
    a = ogf20 [8]

prop_SEQ (Revertible f) g = areEq [f,g::Series 30] "SEQ(f) == I(g)@f + IC(g)@f"

mset :: KnownNat n => Transform n
mset f
  | constant f /= 0 = infty
  | otherwise =
      let term k = (f `o` xpow k) / fromIntegral k
      in exp $ sum $ term <$> [1 .. precision f - 1]

pset :: KnownNat n => Transform n
pset f
  | constant f /= 0 = infty
  | otherwise =
      let term k = (-1)^(k+1) * (f `o` xpow k) / fromIntegral k
      in exp $ sum $ term <$> [1 .. precision f - 1]

prop_MSET (Revertible f) = areSimEq [f::Series 13, mset f] "MSET(f) == g"
prop_PSET (Revertible f) = areSimEq [f::Series 13, pset f] "PSET(f) == g"

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
    , ("unit/BOUS",              check   1 prop_BOUS_u)
    , ("unit/BOUS2",             check   1 prop_BOUS2_u)
    , ("unit/BOUS2i",            check   1 prop_BOUS2i_u)
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
    , ("BOUS2i.BOUS2=id",        check  20 prop_BOUS2i_BOUS2)
    , ("BOUS2.BOUS2i=id",        check  20 prop_BOUS2_BOUS2i)
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
    , ("Derivative of geometric",  check 1 prop_Derivative_of_geometric)
    , ("Product rule",           check  50 prop_Product_rule)
    , ("Reciprocal rule",        check  50 prop_Reciprocal_rule)
    , ("Chain rule",             check  50 prop_Chain_rule)
    , ("Fund Thm of Calc 1",     check 100 prop_Fundamental1)
    , ("Fund Thm of Calc 2",     check 100 prop_Fundamental2)
    , ("Integration by parts",   check 100 prop_Integration_by_parts)
    , ("A235802",                check   1 prop_A235802)
    , ("A001710-1",              check   1 prop_A001710_1)
    , ("A001710-2",              check   1 prop_A001710_2)
    , ("A075834-1",              check   1 prop_A075834_1)
    , ("A075834-2",              check   1 prop_A075834_2)
    , ("A003149",                check   1 prop_A003149)
    , ("Fredholm-Rueppel",       check   1 prop_Fredholm_Rueppel)
    , ("A000670-1",              check   1 prop_A000670_1)
    , ("A000670-2",              check   1 prop_A000670_2)
    , ("A000670-3",              check   1 prop_A000670_3)
    , ("A000670-4",              check   1 prop_A000670_4)
    , ("Increasing forests",     check   1 prop_IncreasingForests)
    , ("Unit circle",            check  20 prop_Unit_circle)
    , ("Exact sqrt",             check  50 prop_Exact_sqrt)
    , ("Derivative of sin",      check  50 prop_Derivative_of_sin)
    , ("Derivative of cos",      check  50 prop_Derivative_of_cos)
    , ("Derivative of tan",      check  50 prop_Derivative_of_tan)
    , ("Derivative of arcsin",   check  50 prop_Derivative_of_arcsin)
    , ("Derivative of arccos",   check  50 prop_Derivative_of_arccos)
    , ("Derivative of arctan",   check  50 prop_Derivative_of_arctan)
    , ("Derivative of sinh",     check  50 prop_Derivative_of_sinh)
    , ("Derivative of cosh",     check  50 prop_Derivative_of_cosh)
    , ("Derivative of tanh",     check  50 prop_Derivative_of_tanh)
    , ("Derivative of arsinh",   check   5 prop_Derivative_of_arsinh)
    , ("Derivative of arcosh",   check   5 prop_Derivative_of_arcosh)
    , ("Derivative of artanh",   check  50 prop_Derivative_of_artanh)
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
