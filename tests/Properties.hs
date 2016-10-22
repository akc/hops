{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
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
import HOPS.Matrix
import HOPS.GF
import qualified HOPS.GF.Const as C
import qualified HOPS.GF.Rats as R
import HOPS.GF.Series
import HOPS.GF.Transform

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

instance Arbitrary Rat where
    arbitrary = frequency
        [ ( 4, return Indet )
        , ( 1, return DZ )
        , (95, Val <$> arbitrary)
        ]

instance KnownNat n => Arbitrary (Series n) where
    arbitrary = series (Proxy :: Proxy n) <$> arbitrary

instance Arbitrary Prg where
    arbitrary = Prg <$> listOf arbitrary

instance Arbitrary Cmd where
    arbitrary = oneof
        [ Expr <$> arbitrary
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
        , (15, EA       <$> arbitrary)
        , ( 3, ETag     <$> arbitrary)
        , (20, EVar     <$> nameGen  )
        , (33, ELit     <$> arbitrary)
        , ( 1, Tr       <$> nameGen <*> arbitrary)
        , ( 7, ERats    <$> ratsGen  )
        , ( 1, Expr0    <$> arbitrary)
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

infix 4 ~=

(~=) :: Series n -> Series n -> Bool
f ~= g = rprefix f == rprefix g
  where
    rprefix = take (precision f - 1) . rationalPrefix

infix 4 ~~=

f ~~= g = as `isPrefixOf` bs || bs `isPrefixOf` as
  where
    as = rationalPrefix f
    bs = rationalPrefix g

coeffs = map Val . rationalPrefix

check :: Testable prop => Int -> prop -> IO Result
check n = quickCheckWithResult stdArgs {maxSuccess = n}

evalPrg :: KnownNat n => Env n -> Prg -> Series n
evalPrg env = evalCorePrg env . core

runPrg :: KnownNat n => Env n -> ByteString -> Series n
runPrg env = evalPrg env . fromMaybe (error "parse error") . parsePrg

ogf :: KnownNat n => Proxy n -> [Integer] -> Series n
ogf n = series n . map (Val . fromIntegral)

poly :: KnownNat n => Proxy n -> [Integer] -> Series n
poly n = polynomial n . map (Val . fromIntegral)

ogf20 = ogf (Proxy :: Proxy 20)
poly20 = poly (Proxy :: Proxy 20)
empty20 = Env V.empty M.empty :: Env 20

ogf40 = ogf (Proxy :: Proxy 40)
poly40 = poly (Proxy :: Proxy 40)
empty40 = Env V.empty M.empty :: Env 40

envFromList :: KnownNat n => [(ByteString, Series n)] -> Env n
envFromList assoc = Env V.empty (M.fromList assoc)

areEq :: ByteString -> ByteString -> [Integer] -> Bool
areEq lhs rhs cs =
    let env = insertVar "f" (ogf20 cs) empty20
    in rationalPrefix (runPrg env lhs) == rationalPrefix (runPrg env rhs)

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

prop_Neg_power_u = areEq "{-(-1)^n}" "-1/(1+x)" []

prop_LEFT_u      = areEq "LEFT      {4,3,2,1}"         "{3,2,1}"               []
prop_RIGHT_u     = areEq "RIGHT     {4,3,2,1}"         "{1,4,3,2,1}"           []
prop_M2_u        = areEq "M2        {5,4,3,2,1}"       "{5,8,6,4,2}"           []
prop_M2i_u       = areEq "M2i       {5,8,6,4,2}"       "{5,4,3,2,1}"           []
prop_AERATE1_u   = areEq "AERATE1   {5,4,3,2,1}"   "{5,0,4,0,3,0,2,0,1,0}"     []
prop_AERATE2_u   = areEq "AERATE2   {1,2,3,4}"     "{1,0,0,2,0,0,3,0,0,4,0,0}" []
prop_BINOMIAL_u  = areEq "BINOMIAL  {1,2,4,8,16}"      "{1,3,9,27,81}"         []
prop_BINOMIALi_u = areEq "BINOMIALi {1,3,9,27,81}"     "{1,2,4,8,16}"          []
prop_BIN1_u      = areEq "BIN1      {2,4,8,16}"        "{2,-8,26,-80}"         []
prop_BISECT0_u   = areEq "BISECT0   {0,1,2,3,4,5}"     "{0,2,4}"               []
prop_BISECT1_u   = areEq "BISECT1   {0,1,2,3,4,5}"     "{1,3,5}"               []
prop_BOUS_u      = areEq "BOUS      {5,4,3,2,1}"       "{1,6,15,32,83,262}"    []
prop_BOUS2_u     = areEq "BOUS2     {5,4,3,2,1}"       "{5,9,16,33,84}"        []
prop_BOUS2i_u    = areEq "BOUS2i    {5,4,3,2,1}"       "{5,-1,0,-5,4}"         []
prop_CATALAN_u   = areEq "CATALAN   {1,1,1,1,1}"       "{1,1,2,5,14}"          []
prop_CATALANi_u  = areEq "CATALANi  {1,1,2,5,14}"      "{1,1,1,1,1}"           []
prop_CYC_u       = areEq "CYC       {0,1,1,1,1,1}"     "{0,1,2,3,5,7}"         []
prop_DIFF_u      = areEq "DIFF      {9,4,1,0,1,4,9}"   "{-5,-3,-1,1,3,5}"      []
prop_INVERT_u    = areEq "INVERT    {1,2,3,4,5}"       "{1,3,8,21,55}"         []
prop_INVERTi_u   = areEq "INVERTi   {1,3,8,21,55}"     "{1,2,3,4,5}"           []
prop_MOBIUS_u    = areEq "MOBIUS    {1,3,4,7,6,12}"    "{1,2,3,4,5,6}"         []
prop_MOBIUSi_u   = areEq "MOBIUSi   {1,2,3,4,5,6}"     "{1,3,4,7,6,12}"        []
prop_EULER_u     = areEq "EULER     {1,1,0,0,0,0,0}"   "{1,2,2,3,3,4,4}"       []
prop_EULERi_u    = areEq "EULERi    {1,2,2,3,3,4,4}"   "{1,1,0,0,0,0,0}"       []
prop_LAH_u       = areEq "LAH       {5,4,3,2,1}"       "{5,4,11,44,229}"       []
prop_LAHi_u      = areEq "LAHi      {5,4,3,2,1}"       "{5,4,-5,8,-11}"        []
prop_EXP_u       = areEq "EXP       {1,2,3,4}"         "{1,3,10,41}"           []
prop_LOG_u       = areEq "LOG       {1,3,10,41}"       "{1,2,3,4}"             []
prop_CONV_u      = areEq "CONV      {1,2,3,4,5}"       "{1,4,10,20,35}"        []
prop_CONVi_u     = areEq "CONVi     {1,4,10,20,35}"    "{1,2,3,4,5}"           []
prop_EXPCONV_u   = areEq "EXPCONV   {1,4,9,16,25}"     "{1,8,50,248,1048}"     []
prop_MSET_u      = areEq "MSET      {0,1,0,1}" "[1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7]" []
prop_NEGATE_u    = areEq "NEGATE    {5,4,3,2,1}"       "{5,-4,-3,-2,-1}"       []
prop_PRODS_u     = areEq "PRODS     {1,2,3,4,5}"       "{1,2,6,24,120}"        []
prop_PSET_u      = areEq "PSET      {1,2,3}" "[1,2,4,6,6,6,4,2,1,0,0,0,0,0,0,0,0,0,0,0]" []
prop_PSUM_u      = areEq "PSUM      {1,2,3,4,5}"       "{1,3,6,10,15}"         []
prop_PSUMSIGN_u  = areEq "PSUMSIGN  {1,2,3,4,5}"       "{1,1,2,2,3}"           []
prop_REVERT_u    = areEq "REVERT    {1,2,3,4,5}"       "{1,-2,5,-14,42}"       []
prop_REVEGF_u    = areEq "REVEGF    {1,2,3,4,5}"       "{1,-4,39,-616,13505}"  []
prop_SEQ_u       = areEq "SEQ       {0,1,1,0,0,0}"     "{1,1,2,3,5,8}"
prop_STIRLING_u  = areEq "STIRLING  {1,2,3,4,5}"       "{1,3,10,37,151}"       []
prop_STIRLINGi_u = areEq "STIRLINGi {1,3,10,37,151}"   "{1,2,3,4,5}"           []
prop_TRISECT0_u  = areEq "TRISECT0  {0,1,2,3,4,5,6}"   "{0,3,6}"               []
prop_TRISECT1_u  = areEq "TRISECT1  {0,1,2,3,4,5,6}"   "{1,4}"                 []
prop_TRISECT2_u  = areEq "TRISECT2  {0,1,2,3,4,5,6}"   "{2,5}"                 []
prop_POINT_u     = areEq "POINT     {1,1,4,27,256}"    "{0,1,8,81,1024}"       []
prop_WEIGHT_u    = areEq "WEIGHT    {1,1,1,1,1,1,1,1}" "{1,1,2,2,3,4,5,6}"     []
prop_PARTITION_u = areEq "PARTITION {1,3,5}" "[1,1,1,2,2,3,4,4,5,6,7,8,9,10,11,13,14,15,17,18]" []
prop_HANKEL_u    = areEq "HANKEL    {6,5,4,3,2,1}"     "{6,-1,0,0}"            []
prop_lHANKEL_u   = areEq "lHANKEL   {1,4,9,16,25,36}"  "{7,17,31,49}"          []
prop_I_u         = areEq "I         {2,4}"             "[0,0,1,0,1]"
prop_IC_u        = areEq "IC        {2,4}"   "[1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]" []
prop_LEFT                  = areEq "LEFT(f)" "D(f./{n!}) .* {n!}"
prop_RIGHT              cs = not (null cs) ==> areEq "RIGHT(f)" "1 + x*f" cs
prop_M2i_M2                = areEq "M2i(M2(f))" "f"
prop_BINOMIALi_BINOMIAL    = areEq "BINOMIALi(BINOMIAL(f))" "f"
prop_BINOMIAL_BINOMIALi    = areEq "BINOMIAL(BINOMIALi(f))" "f"
prop_BOUS2i_BOUS2          = areEq "BOUS2i(BOUS2(f))" "f"
prop_BOUS2_BOUS2i          = areEq "BOUS2(BOUS2i(f))" "f"
prop_CATALANi_CATALAN      = areEq "CATALANi(CATALAN(f))" "f"
prop_CATALAN_CATALANi      = areEq "CATALAN(CATALANi(f))" "f"
prop_INVERTi_INVERT     cs = areEq "INVERTi(INVERT(f))" "f" (take 19 cs)
prop_INVERT_INVERTi     cs = areEq "INVERT(INVERTi(f))" "f" (take 19 cs)
prop_LAHi_LAH           cs = areEq "LAHi(LAH(f))" "f" (take 10 cs)
prop_LAH_LAHi           cs = areEq "LAH(LAHi(f))" "f" (take 10 cs)
prop_EULERi_EULER       cs = areEq "EULERi(EULER(f))" "f" (take 6 cs)
prop_EULER_EULERi       cs = areEq "EULER(EULERi(f))" "f" (take 6 cs)
prop_LOG_EXP            cs = areEq "LOG(EXP(f))" "f" (take 15 cs)
prop_EXP_LOG            cs = areEq "EXP(LOG(f))" "f" (take 15 cs)
prop_MOBIUSi_MOBIUS        = areEq "MOBIUSi(MOBIUS(f))" "f"
prop_MOBIUS_MOBIUSi        = areEq "MOBIUS(MOBIUSi(f))" "f"
prop_CONVi_CONV         cs = areEq "CONVi(CONV(f))" "abs(f)" (take 10 cs)
prop_STIRLINGi_STIRLING cs = areEq "STIRLINGi(STIRLING(f))" "f" (take 10 cs)
prop_STIRLING_STIRLINGi cs = areEq "STIRLING(STIRLINGi(f))" "f" (take 10 cs)
prop_NEGATE_involutive     = areEq "NEGATE(NEGATE(f))" "f"
prop_BIN1_involutive    cs = areEq "BIN1(BIN1(f))" "f" (take 8 cs)
prop_BISECT0_AERATE1    cs = areEq "BISECT0(AERATE1(f))" "f" (take 10 cs)
prop_TRISECT0_AERATE2   cs = areEq "TRISECT0(AERATE2(f))" "f" (take 6 cs)

prop_M2        = areEq "M2(f)"        "2*f - f(0)"
prop_M2i       = areEq "M2i(f)"       "(f + f(0))/2"
prop_AERATE1   = areEq "AERATE1(f)"   "f(x^2)"
prop_AERATE2   = areEq "AERATE2(f)"   "f(x^3)"
prop_BINOMIAL  = areEq "BINOMIAL(f)"  "(f ./ {n!}) * {1/n!} .* {n!}"
prop_BINOMIALi = areEq "BINOMIALi(f)" "(f ./ {n!}) * {(-1)^n/n!} .* {n!}"
prop_BIN1      = areEq "BIN1(f)" "LEFT((-{(-1)^n/n!} * (((x*f) ./ {n!})@(-x))) .* {n!})"
prop_DIFF      = areEq "DIFF(f)"      "(D(f./{n!}) .* {n!} - f)"
prop_INVERT    = areEq "INVERT(f)"    "g=1/(1-x*f);LEFT(g)"
prop_INVERTi   = areEq "INVERTi(f)"   "g=-1/(1+x*f);LEFT(g)"
prop_LAH       = areEq "LAH(f)"       "(f./{n!})@(x/(1-x)) .* {n!}"
prop_LAHi      = areEq "LAHi(f)"      "(f./{n!})@(x/(1+x)) .* {n!}"
prop_EXP       = areEq "EXP(f)"       "(({1/n!}@(x*f./{n!}) - 1) .* {n!})/x"
prop_LOG       = areEq "LOG(f)"       "({0,(-1)^(n+1)/n}@(x*f./{n!}) .* {n!})/x"
prop_CONV      = areEq "CONV(f)"      "f^2"
prop_CONVi  cs = areEq "CONVi(f^2)"   "abs(f)" (take 10 cs)
prop_EXPCONV   = areEq "EXPCONV(f)"   "(f./{n!})^2 .* {n!}"
prop_NEGATE    = areEq "NEGATE(f)"    "2*f(0) - f"
prop_PSUM      = areEq "PSUM(f)"      "f/(1-x)"
prop_PSUMSIGN  = areEq "PSUMSIGN(f)"  "f/(1+x)"
prop_STIRLING  = areEq "STIRLING(f)"  "((x*f ./ {n!})@({0,1/n!}) .* {n!})/x"
prop_STIRLINGi = areEq "STIRLINGi(f)" "((x*f ./ {n!})@({0,(-1)^(n+1)/n}) .* {n!})/x"
prop_POINT     = areEq "POINT(f)"     "x*D(f./{n!}) .* {n!}"

prop_Distrib1 :: Series 20 -> Series 20 -> Series 20 -> Bool
prop_Distrib1 f g h = f*(g+h) == f*g + f*h

prop_Distrib2 :: Series 20 -> Series 20 -> Series 20 -> Bool
prop_Distrib2 f g h = f.*(g+h) == f.*g + f.*h

prop_Distrib3 :: Series 20 -> Series 20 -> Series 20 -> Bool
prop_Distrib3 f g h = (g+h)./f == g./f + h./f

prop_DISTRIB as bs cs =
    and [ exec lhs ~~= exec rhs | (lhs, rhs) <- eqs ]
  where
    exec = runPrg $ envFromList [("f", ogf20 as), ("g", ogf20 bs), ("h", ogf20 cs)]
    eqs = [ ("f*(g+h)", "f*g + f*h")
          , ("f.*(g+h)", "f.*g + f.*h")
          , ("(g+h)./f", "g./f + h./f")
          ]

prop_Compose cs = areEq "x@f" "f" cs && areEq "f(x)" "f" cs

prop_f cs = let f = ogf20 cs in runPrg (envFromList [("f", f)]) "f" == f

prop_Powers_of_2 = areEq "{2^n}" "f" cs && areEq "1/(1-2*x)" "f" cs
  where
    cs = [ 2^n | n<-[0..] ]

a000110 =
    [ 1,1,2,5,15,52,203,877,4140,21147,115975,678570,4213597,27644437,190899322
    , 1382958545,10480142147,82864869804,682076806159,5832742205057,51724158235372
    , 474869816156751,4506715738447323,44152005855084346,445958869294805289
    , 4638590332229999353,49631246523618756274
    ]

prop_Bell_1 =
    let f = runPrg empty40 "{1/n!} @ {0,1/n!} .* {n!}"
    in take (length a000110) (coeffs f) == a000110

prop_Bell_2 =
    let f = runPrg empty40 "e = 1 + integral(e); e(e-1) .* {n!}"
    in take (length a000110) (coeffs f) == a000110

prop_Bell_3 =
    let f = runPrg empty40 "y = x/(1-x); B = 1 + y*B(y)"
    in take (length a000110) (coeffs f) == a000110

prop_Bell_4 =
    let f = runPrg empty40 "Bell = 1+integral(exp(x)*Bell); Bell .* {n!}"
    in take (length a000110) (coeffs f) == a000110

-- Bessel numbers
a006789 = [ 1,1,2,5,14,43,143,509,1922,7651,31965,139685,636712,3020203,14878176
          , 75982829,401654560,2194564531,12377765239,71980880885,431114329728
          , 2656559925883,16825918195484,109439943234749,730365368850192
          ]

prop_Bessel =
    let f = runPrg empty40 "B=1/(1-x-x^2*B(x/(1-x))/(1-x))"
    in take (length a006789) (coeffs f) == a006789

-- Number of ballots / ordered set partitions
a000670 =
    [ 1,1,3,13,75,541,4683,47293,545835,7087261,102247563,1622632573,28091567595
    , 526858348381,10641342970443,230283190977853,5315654681981355
    , 130370767029135901,3385534663256845323,92801587319328411133
    , 2677687796244384203115
    ]

prop_A000670_1 = take (length a000670) (coeffs f) == a000670
  where
    f = runPrg empty40 "1/(2-exp(x)) .* {n!}"

prop_A000670_2 = take (length a000670) (coeffs f) == a000670
  where
    f = runPrg empty40 "y = 1 + integral(2*y^2 - y); y .* {n!}"

prop_A000670_3 = take (length a000670) (coeffs f) == a000670
  where
    f = runPrg empty40 "1 + x*STIRLING({(n+1)!})"

prop_A000670_4 = take (length a000670) (coeffs f) == a000670
  where
    f = runPrg empty40 "A = integral(1+3*A+2*A^2); (1+A) .* {n!}"

-- Catalan numbers
a000108 =
    [ 1,1,2,5,14,42,132,429,1430,4862,16796,58786,208012,742900,2674440,9694845
    , 35357670,129644790,477638700,1767263190,6564120420,24466267020,91482563640
    , 343059613650,1289904147324,4861946401452,18367353072152,69533550916004
    , 263747951750360,1002242216651368,3814986502092304
    ]

prop_Catalan_1 = take (length a000108) (coeffs f) == a000108
  where
    f = runPrg empty40 "{(2*n)!/(n!*(n+1)!)}"

prop_Catalan_2 = take (length a000108) (coeffs f) == a000108
  where
    f = runPrg empty40 "(1-sqrt(1-4*x))/(2*x)"

prop_Catalan_3 = take 12 (coeffs f) == take 12 a000108
  where
    f = runPrg empty20 cf
    cf = "1/(1-x/(1-x/(1-x/(1-x/(1-x/(1-x/(1-x/(1-x/(1-x/(1-x/(1-x)))))))))))"

prop_Catalan_4 = take (length a000108) (coeffs f) == a000108
  where
    f = runPrg empty40 "C = 1 + x*C^2"

a001006 =
    [ 1,1,2,4,9,21,51,127,323,835,2188,5798,15511,41835,113634,310572,853467
    , 2356779,6536382,18199284,50852019,142547559,400763223,1129760415
    , 3192727797,9043402501,25669818476,73007772802,208023278209,593742784829
    ]

prop_Motzkin_1 = take (length a001006) (coeffs f) == a001006
  where
    f = runPrg empty40 "(1-x-(1-2*x-3*x^2)^(1/2))/(2*x^2)"

prop_Motzkin_2 = take (length a001006) (coeffs f) == a001006
  where
    g = ogf40 $ mapMaybe (fmap numerator . maybeRational) a000108
    f = runPrg (envFromList [("f", g)]) "(f(x/(1+x))-1)/x"

prop_Motzkin_3 = take 13 (coeffs f) == take 13 a001006
  where
    f = runPrg empty20 cf
    cf = "1/(1-x-x^2/(1-x-x^2/(1-x-x^2/(1-x-x^2/(1-x-x^2/(1-x-x^2))))))"

prop_Motzkin_4 = take (length a001006) (coeffs f) == a001006
  where
    f = runPrg empty40 "M = 1 + x*M + x^2*M^2"

prop_Euler = take (length ans) (coeffs f) == ans
  where
    f = runPrg empty20 "2/({1/n!} + {(-1)^n/n!}) .* {n!}"
    ans = [1,0,-1,0,5,0,-61,0,1385,0,-50521,0,2702765,0,-199360981,0,19391512145]

fibonacci =
    [ 1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946
    , 17711,28657,46368,75025,121393,196418,317811,514229,832040,1346269
    , 2178309,3524578,5702887,9227465,14930352,24157817,39088169
    ]

prop_Fibonacci_1 = take (length fibonacci) (coeffs f) == fibonacci
  where
    f = runPrg empty40 "1/(1-x-x^2)"

prop_Fibonacci_2 = take (length fibonacci) (coeffs f) == fibonacci
  where
    f = runPrg empty40 "f = 1 + (x+x^2)*f"

dropTrailingZeros = reverse . dropWhile (== 0) . reverse

prop_ListOfCoeffs cs = not (null cs) ==> f == ogf20 cs
  where
    f = runPrg empty20 (B.pack (toRatsString cs))

prop_Polynomial cs = not (null cs) ==> f == poly20 cs
  where
    f = runPrg empty20 (B.pack p)
    p = intercalate "+" (zipWith (\c i -> show c ++ "*x^" ++ show i) cs [0..])

prop_AP a b = f == ogf20 [a,b..]
  where
    f = runPrg empty20 (B.pack expr)
    expr = concat ["{", show a, ",", show b, ",...}"]

prop_Geometric c = f1 == g && g == f2
  where
    g  = ogf20 [ c^i | i <- [0..] ]
    f1 = runPrg empty20 $ B.pack $ printf "{(%s)^n}" (show c)
    f2 = runPrg empty20 $ B.pack $ printf "1/(1 - %s*x)" (show c)

a000272 =
    [ 1,1,1,3,16,125,1296,16807,262144,4782969,100000000,2357947691
    , 61917364224,1792160394037,56693912375296,1946195068359375
    , 72057594037927936,2862423051509815793,121439531096594251776
    ]

prop_Labeled_trees_1 = take (length a000272) lhs == a000272
  where
    lhs = coeffs (runPrg empty20 "{1,1,n^(n-2)}")

prop_Labeled_trees_2 = take (length a000272) lhs == a000272
  where
    lhs = coeffs (runPrg empty20 "T = x*exp(T); f = 1+T-(1/2)*T^2; f .* {n!}")

a000081 =
    [ 0,1,1,2,4,9,20,48,115,286,719,1842,4766,12486,32973,87811,235381,634847
    , 1721159,4688676,12826228,35221832,97055181,268282855,743724984,2067174645
    , 5759636510,16083734329,45007066269,126186554308,354426847597
    ]

a000055 =
    [ 1,1,1,1,2,3,6,11,23,47,106,235,551,1301,3159,7741,19320,48629,123867,317955
    , 823065,2144505,5623756,14828074,39299897,104636890,279793450,751065460
    , 2023443032,5469566585,14830871802
    ]

prop_Unlabeled_trees =
    runPrg env "1 + f - f^2/2 + f(x^2)/2" == ans
  where
    env = envFromList [("f", inp)]
    inp = ogf40 a000081
    ans = ogf40 a000055

a006125 =
    [ 1,1,2,8,64,1024,32768,2097152,268435456,68719476736,35184372088832
    , 36028797018963968,73786976294838206464,302231454903657293676544
    , 2475880078570760549798248448,40564819207303340847894502572032
    ]

prop_Labeled_graphs = take (length a006125) lhs == a006125
  where
    lhs = coeffs (runPrg empty20 "{2^(n*(n-1)/2)}")

prop_Connected_labeled_graphs =
    runPrg env "1 + {0,(-1)^(n+1)/n} @ (f ./ {n!}) .* {n!}" == ans
  where
    env = envFromList [("f", inp)]
    inp = ogf20 [0,1,2,8,64,1024,32768,2097152,268435456,68719476736,35184372088832]
    ans = ogf20 [1,1,1,4,38,728,26704,1866256,251548592,66296291072,34496488594816]

-- D(1/(1-x)) = (1/(1-x))^2
prop_Derivative_of_geometric = u == v
  where
    u = V.init $ coeffVector (runPrg empty20 "D(1/(1-x))")
    v = V.init $ coeffVector (runPrg empty20 "(1/(1-x))^2")

-- D(f*g) = D(f)*g + f*D(g)
prop_Product_rule cs bs =
    not (null cs) && not (null (bs :: [Integer])) ==>
        exec (B.pack (printf "D(f*%s)" bs')) ==
        exec (B.pack (printf "D(f)*%s + f*D(%s)" bs' bs'))
      where
        bs' = toRatsString bs
        exec = runPrg $ envFromList [("f", ogf20 cs)]

-- D(1/f) = -D(f)/f^2
prop_Reciprocal_rule c cs =
    c /= 0 ==>
        let exec = runPrg $ envFromList [("f", ogf20 (c:cs))]
        in exec "D(1/f)" == exec "-D(f)/f^2"

-- D(g@f) = D(g)@f * D(f)
prop_Chain_rule cs bs =
    not (null (bs :: [Integer])) ==>
        exec (B.pack (printf "D(%s@f)" (toRatsString bs))) ==
        exec (B.pack (printf "(D(%s))@f * D(f)" (toRatsString bs)))
      where
        exec = runPrg $ envFromList [("f", ogf20 (0:cs))]

-- Fundametal Theorem of Calculus
prop_Fundamental cs =
    exec "f(0) + integral(D(f))" == f && f == exec "D(integral(f))"
  where
    f = ogf40 (take 39 cs)
    exec = runPrg $ envFromList [("f", f)]

-- Integration by parts
prop_Integration_by_parts cs bs =
    let exec = runPrg (envFromList [("f", ogf20 cs), ("g", ogf20 bs)])
    in exec "f*g" == exec "(f*g)@0 + integral(D(f)*g) + integral(f*D(g))"

-- Order of the alternating group A_n
a001710 =
    [ 1,1,1,3,12,60,360,2520,20160,181440,1814400,19958400,239500800,3113510400
    , 43589145600,653837184000,10461394944000,177843714048000,3201186852864000
    , 60822550204416000,1216451004088320000
    ]

prop_A001710_1 = take (length a001710) lhs == a001710
  where
    lhs = coeffs (runPrg empty40 "(2-x^2)/(2-2*x) .* {n!}")

prop_A001710_2 = take (length a001710) lhs == a001710
  where
    lhs = coeffs (runPrg empty40 "{1,1,n!/2}")

a235802 =
    [ 1,1,3,12,61,375,2697,22176,204977,2102445,23685615,290642220,3857751573
    , 55063797243,840956549517,13682498891040,236257301424225,4314883836968505
    , 83102361300891963,1683252077760375660,35770269996769203405
    ]

prop_A235802 = take (length a235802) lhs == a235802
  where
    lhs = coeffs (runPrg empty40 "1/(1 - x)^(2/(2-x)) .* {n!}")

a075834 =
    [ 1,1,1,2,7,34,206,1476,12123,111866,1143554,12816572,156217782,2057246164
    , 29111150620,440565923336,7101696260883,121489909224618,2198572792193786
    , 41966290373704332,842706170872913634
    ]

prop_A075834_1 = take (length a075834) lhs == a075834
  where
    lhs = coeffs (runPrg empty40 "A = 1 + x/(1 - x*D(A)/A)")

prop_A075834_2 = areEq "A = 1 + x/(1 - x*D(A)/A)" "A = 1 + x/(1 - (x*D(A))/A)" []

a003149 =
    [ 1,2,5,16,64,312,1812,12288,95616,840960,8254080,89441280,1060369920
    , 13649610240,189550368000,2824077312000,44927447040000,760034451456000
    , 13622700994560000,257872110354432000,5140559166898176000
    ]

prop_A003149 = take (length a003149) lhs == a003149
  where
    lhs = coeffs (runPrg empty40 "(log(1-x)/(x/2-1) .* {n!})/x")

-- Fredholm-Rueppel sequence
a036987 =
    [ 1,1,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    , 0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    ]

prop_Fredholm_Rueppel_1 = take (length a036987) lhs == a036987
  where
    env = Env V.empty M.empty :: Env 60
    lhs = coeffs (runPrg env "f=1+x*AERATE1(f)")

prop_Fredholm_Rueppel_2 = take (length a036987) lhs == a036987
  where
    env = Env V.empty M.empty :: Env 60
    lhs = coeffs (runPrg env "f=1+x*f(x^2)")

-- XXX: Dirichlet g.f. for right-shifted sequence: 2^(-s)/(1-2^(-s)).
-- prop_Fredholm_Rueppel_3

prop_IncreasingForests =
    ogf40 [1,1..] == runPrg empty40 "tree = integral(forest); forest = exp(tree)"

prop_Unit_circle c cs = c /= 0 ==>
    runPrg (envFromList [("f", poly40 (0:c:cs))]) "cos(f)^2 + sin(f)^2" == poly40 [1]

prop_Exact_sqrt c d =
    let prg = B.pack ("sqrt({(" ++ show c ++ ")^2/(" ++ show d ++ ")^2})")
    in runPrg empty20 prg ~= ogf20 [abs c::Integer] / ogf20 [abs d::Integer]

prop_Derivative_of_sin cs = any (/=0) cs ==>
    let env = envFromList [("f", poly20 (0:cs))]
    in runPrg env "D(sin(f))" ~= runPrg env "D(f)*cos(f)"

prop_Derivative_of_cos cs = any (/=0) cs ==>
    let env = envFromList [("f", poly20 (0:cs))]
    in runPrg env "D(cos(f))" ~= runPrg env "-D(f)*sin(f)"

prop_Derivative_of_tan cs = any (/=0) cs ==>
    let env = envFromList [("f", poly20 (0:cs))]
    in runPrg env "D(tan(f))" ~= runPrg env "D(f)/cos(f)^2"

prop_Derivative_of_arcsin cs = any (/=0) cs ==>
    let env = envFromList [("f", poly20 (0:cs))]
    in runPrg env "D(arcsin(f))" ~= runPrg env "D(f)/sqrt(1-f^2)"

prop_Derivative_of_arccos cs = any (/=0) cs ==>
    let env = envFromList [("f", poly20 (0:cs))]
    in runPrg env "D(arccos(f))" ~= runPrg env "-D(f)/sqrt(1-f^2)"

prop_Derivative_of_arctan cs = any (/=0) cs ==>
    let env = envFromList [("f", poly20 (0:cs))]
    in runPrg env "D(arctan(f))" ~= runPrg env "D(f)/(1+f^2)"

prop_Derivative_of_sinh cs = any (/=0) cs ==>
    let env = envFromList [("f", poly20 (0:cs))]
    in runPrg env "D(sinh(f))" ~= runPrg env "D(f)*cosh(f)"

prop_Derivative_of_cosh cs = any (/=0) cs ==>
    let env = envFromList [("f", poly20 (0:cs))]
    in runPrg env "D(cosh(f))" ~= runPrg env "D(f)*sinh(f)"

prop_Derivative_of_tanh cs = any (/=0) cs ==>
    let env = envFromList [("f", poly20 (0:cs))]
    in runPrg env "D(tanh(f))" ~= runPrg env "D(f)*(1-tanh(f)^2)"

prop_Derivative_of_arsinh cs = any (/=0) cs ==>
    let env = envFromList [("f", poly20 (0:cs))]
    in runPrg env "D(arsinh(f))" ~= runPrg env "D(f)/sqrt(1+f^2)"

prop_Derivative_of_arcosh cs = any (/=0) cs ==>
    let env = envFromList [("f", poly20 (0:cs))]
    in runPrg env "D(arcosh(f))" ~= runPrg env "D(f)/sqrt(f^2-1)"

prop_Derivative_of_artanh cs = any (/=0) cs ==>
    let env = envFromList [("f", poly20 (0:cs))]
    in runPrg env "D(artanh(f))" ~= runPrg env "D(f)/(1-f^2)"

prop_sinh cs =
    let env = envFromList [("f", poly20 (0:cs))]
    in runPrg env "sinh(f)" ~= runPrg env "(exp(f)-exp(-f))/2"

prop_cosh cs =
    let env = envFromList [("f", poly20 (0:cs))]
    in runPrg env "cosh(f)" ~= runPrg env "(exp(f)+exp(-f))/2"

prop_tanh cs =
    let env = envFromList [("f", poly20 (0:cs))]
    in runPrg env "tanh(f)" ~= runPrg env "(exp(f)-exp(-f))/(exp(f)+exp(-f))"

prop_Hyperbolic_unit cs =
    runPrg (envFromList [("f", poly40 (0:cs))]) "cosh(f)^2 - sinh(f)^2" == poly40 [1]

prop_arsinh cs = not (null cs) ==>
    let env = envFromList [("f", poly20 (0:cs))]
    in runPrg env "arsinh(f)" ~= runPrg env "log(f + sqrt(f^2 + 1))"

prop_artanh cs = not (null cs) ==>
    let env = envFromList [("f", poly20 (0:cs))]
    in runPrg env "artanh(f)" ~= runPrg env "(log(1+f) - log(1-f))/2"

a012259 =
    [ 1, 1, 1, 5, 17, 121, 721, 6845, 58337, 698161, 7734241, 111973685, 1526099057
    , 25947503401, 419784870961, 8200346492525, 153563504618177, 3389281372287841
    , 72104198836466881, 1774459993676715365, 42270463533824671697
    , 1147649139272698443481
    ]

prop_A012259_1 = take (length a012259) (coeffs f) == a012259
  where
    f = runPrg empty40 "exp(artanh(tan(x))) .* {n!}"

prop_A012259_2 = take (length a012259) (coeffs f) == a012259
  where
    f = runPrg empty40 "sqrt(sec(2*x)+tan(2*x)) .* {n!}"

prop_A012259_3 = take (length a012259) (coeffs f) == a012259
  where
    f = runPrg empty40 "sqrt((1 + tan(x))/(1 - tan(x))) .* {n!}"

a202152 =
    [ 1,1,1,7,13,101,361,2269,18201,48817,1436401,-2283269,157443397,-826037939
    , 21355181849,-160556822999,3084325024561,-22223879489055,291212769688417
    , 2180748026158255,-118745486165378819,4884619264768661461,-140063412525642293687
    , 4020051993317128467029
    ]

prop_A202152 = take (length a202152) (coeffs f) == a202152
  where
    f = runPrg empty40 "exp(x*(1+x)^x) .* {n!}"

a191422 =
    [ 1,0,2,3,-4,90,-126,-840,21104,-137592,-88920,15741000,-197234808,535289040
    , 25582565904,-522317151720,3223601137920,75590725210560,-2388641226278976
    , 23718732310200960,361277667059425920
    ]

prop_A191422 = take (length a191422) (coeffs f) == a191422
  where
    f = runPrg empty40 "(1+x+x^2)^x .* {n!}"

a053492 =
    [ 1,2,15,184,3155,69516,1871583,59542064,2185497819,90909876100,4226300379983
    , 217152013181544,12219893000227107,747440554689309404,49374719534173925055
    , 3503183373320829575008,265693897270211120103563,21451116469521758657525748
    ]

prop_A053492 = take (length a053492) (coeffs f) == a053492
  where
    f = runPrg empty20 "REVEGF(2 - 1/(1-x))"

a066397 =
    [ 1,-2,6,-20,50,168,-4732,54024,-356670,-1558040,106069172,-2197188864
    , 26605890220,22266781600,-12120090377400,402165029201744,-7732409047854318
    , 38209542402620232,4126723132306766900
    ]

prop_A066397 = take (length a066397) (coeffs f) == a066397
  where
    f = runPrg empty20 "cat=1+x*cat^2;REVEGF(cat)"

a088789 =
    [ 0,1,1,3,14,90,738,7364,86608,1173240,17990600,308055528,5826331440
    , 120629547584,2713659864832,65909241461760,1718947213795328
    , 47912968352783232,1421417290991105664
    ]

prop_A088789 = take (length a088789) (coeffs f) == a088789
  where
    f = runPrg empty20 "f = 2*x/(1+exp(x)); laplace(revert(f))"

a049140 =
    [ 1,1,2,6,20,70,256,969,3762,14894,59904,244088,1005452,4180096
    , 17516936,73913705,313774854,1339162028,5742691704
    ]

prop_A049140 = take (length a049140) (coeffs f) == a049140
  where
    f = runPrg empty20 "REVERT(1 - x - x^3)"

a008965 =
    [ 1, 2, 3, 5, 7, 13, 19, 35, 59, 107, 187, 351, 631, 1181, 2191, 4115
    , 7711, 14601, 27595
    ]

prop_A008965 = take (length a008965) (tail (coeffs f)) == a008965
  where
    f = runPrg empty20 "CYC(I({n+1}))"

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

prop_Coefficients = coeffs f == r && coeffs g == r && coeffs h == [8]
  where
    r = [1,3,8,21,55,144,377,987,2584,6765]
    f = runPrg empty20 "(1/(1-[0,1,1]))?[2*n+1]"
    g = runPrg empty20 "(1/(1-[0,1,1]))?{2*n+1}"
    h = runPrg empty20 "(1/(1-[0,1,1]))?5"

tests =
    [ ("Prg-monoid/id-1",        check 100 prop_Prg_id1)
    , ("Prg-monoid/id-2",        check 100 prop_Prg_id2)
    , ("Prg-monoid/associative", check 100 prop_Prg_assoc)
    , ("Prg-monoid/value",       check 100 prop_Prg_value)
    , ("unit/Rat-Power",         check   1 prop_Rat_power_u)
    , ("unit/Neg-Power",         check   1 prop_Neg_power_u)
    , ("unit/LEFT",              check   1 prop_LEFT_u)
    , ("unit/RIGHT",             check   1 prop_RIGHT_u)
    , ("unit/M2",                check   1 prop_M2_u)
    , ("unit/M2i",               check   1 prop_M2i_u)
    , ("unit/AERATE1",           check   1 prop_AERATE1_u)
    , ("unit/AERATE2",           check   1 prop_AERATE2_u)
    , ("unit/BINOMIAL",          check   1 prop_BINOMIAL_u)
    , ("unit/BINOMIALi",         check   1 prop_BINOMIALi_u)
    , ("unit/BIN1",              check   1 prop_BIN1_u)
    , ("unit/BISECT0",           check   1 prop_BISECT0_u)
    , ("unit/BISECT1",           check   1 prop_BISECT1_u)
    , ("unit/CATALAN",           check   1 prop_CATALAN_u)
    , ("unit/CATALANi",          check   1 prop_CATALANi_u)
    , ("unit/CYC",               check   1 prop_CYC_u)
    , ("unit/DIFF",              check   1 prop_DIFF_u)
    , ("unit/INVERT",            check   1 prop_INVERT_u)
    , ("unit/INVERTi",           check   1 prop_INVERTi_u)
    , ("unit/MOBIUS",            check   1 prop_MOBIUS_u)
    , ("unit/MOBIUSi",           check   1 prop_MOBIUSi_u)
    , ("unit/BOUS",              check   1 prop_BOUS_u)
    , ("unit/BOUS2",             check   1 prop_BOUS2_u)
    , ("unit/BOUS2i",            check   1 prop_BOUS2i_u)
    , ("unit/EULER",             check   1 prop_EULER_u)
    , ("unit/EULERi",            check   1 prop_EULERi_u)
    , ("unit/INVERT",            check   1 prop_INVERT_u)
    , ("unit/INVERTi",           check   1 prop_INVERTi_u)
    , ("unit/LAH",               check   1 prop_LAH_u)
    , ("unit/LAHi",              check   1 prop_LAHi_u)
    , ("unit/EXP",               check   1 prop_EXP_u)
    , ("unit/LOG",               check   1 prop_LOG_u)
    , ("unit/CONV",              check   1 prop_CONV_u)
    , ("unit/CONVi",             check   1 prop_CONVi_u)
    , ("unit/EXPCONV",           check   1 prop_EXPCONV_u)
    , ("unit/MSET",              check   1 prop_MSET_u)
    , ("unit/NEGATE",            check   1 prop_NEGATE_u)
    , ("unit/PSET",              check   1 prop_PSET_u)
    , ("unit/PRODS",             check   1 prop_PRODS_u)
    , ("unit/PSUM",              check   1 prop_PSUM_u)
    , ("unit/PSUMSIGN",          check   1 prop_PSUMSIGN_u)
    , ("unit/REVERT",            check   1 prop_REVERT_u)
    , ("unit/REVEGF",            check   1 prop_REVEGF_u)
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
    , ("M2i.M2=id",              check 100 prop_M2i_M2)
    , ("BINOMIALi.BINOMIAL=id",  check  40 prop_BINOMIALi_BINOMIAL)
    , ("BINOMIAL.BINOMIALi=id",  check  40 prop_BINOMIAL_BINOMIALi)
    , ("BOUS2i.BOUS2=id",        check  20 prop_BOUS2i_BOUS2)
    , ("BOUS2.BOUS2i=id",        check  20 prop_BOUS2_BOUS2i)
    , ("CATALAN.CATALANi=id",    check  20 prop_CATALAN_CATALANi)
    , ("CATALANi.CATALAN=id",    check  20 prop_CATALANi_CATALAN)
    , ("INVERTi.INVERT=id",      check   5 prop_INVERTi_INVERT)
    , ("INVERT.INVERTi=id",      check   5 prop_INVERT_INVERTi)
    , ("LAHi.LAH=id",            check   5 prop_LAHi_LAH)
    , ("LAH.LAHi=id",            check   5 prop_LAH_LAHi)
    , ("EULERi.EULER=id",        check   5 prop_EULERi_EULER)
    , ("EULER.EULERi=id",        check   5 prop_EULER_EULERi)
    , ("LOG.EXP=id",             check 100 prop_LOG_EXP)
    , ("EXP.LOG=id",             check 100 prop_EXP_LOG)
    , ("MOBIUSi_MOBIUS=id",      check 100 prop_MOBIUSi_MOBIUS)
    , ("MOBIUS.MOBIUSi=id",      check 100 prop_MOBIUS_MOBIUSi)
    , ("CONVi.CONV=id",          check  40 prop_CONVi_CONV)
    , ("STIRLINGi_STIRLING=id",  check 100 prop_STIRLINGi_STIRLING)
    , ("STIRLING.STIRLINGi=id",  check 100 prop_STIRLING_STIRLINGi)
    , ("NEGATE/involutive",      check 100 prop_NEGATE_involutive)
    , ("BIN1/involutive",        check 100 prop_BIN1_involutive)
    , ("BISECT0.AERATE1=id",     check 100 prop_BISECT0_AERATE1)
    , ("TRISECT0_AERATE2=id",    check 100 prop_TRISECT0_AERATE2)
    , ("M2 = 2*f - f(0)",        check 100 prop_M2)
    , ("M2i = (f + f(0))/2",     check 100 prop_M2i)
    , ("AERATE1 = f(x^2)",       check 100 prop_AERATE1)
    , ("AERATE2 = f(x^3)",       check 100 prop_AERATE2)
    , ("BINOMIAL = expr",        check  50 prop_BINOMIAL)
    , ("BINOMIALi = expr",       check  50 prop_BINOMIALi)
    , ("BIN1 = expr",            check 100 prop_BIN1)
    , ("DIFF = expr",            check 100 prop_DIFF)
    , ("LAH  = (f./{n!})@(x/(1-x)) .* {n!}", check 100 prop_LAH)
    , ("LAHi = (f./{n!})@(x/(1+x)) .* {n!}", check 100 prop_LAHi)
    , ("EXP = expr",             check 100 prop_EXP)
    , ("LOG = expr",             check 100 prop_LOG)
    , ("CONV = f^2",             check 100 prop_CONV)
    , ("CONVi = sqrt(f)",        check   2 prop_CONVi)
    , ("EXPCONV = (f./{n!})^2 .* {n!}", check 100 prop_EXPCONV)
    , ("NEGATE = 2*f(0) - f",    check 100 prop_NEGATE)
    , ("PSUM = f/(1-x)",         check 100 prop_PSUM)
    , ("PSUMSIGN = f/(1+x)",     check 100 prop_PSUMSIGN)
    , ("STIRLING = expr",        check 100 prop_STIRLING)
    , ("STIRLINGi = expr",       check 100 prop_STIRLINGi)
    , ("POINT = x*D(f./{n!}) .* {n!}", check 100 prop_POINT)
    , ("distributive1",          check 100 prop_Distrib1)
    , ("distributive2",          check 100 prop_Distrib2)
    , ("distributive3",          check 100 prop_Distrib3)
    , ("DISTRIBUTIVE",           check 100 prop_DISTRIB)
    , ("eval: f",                check 100 prop_f)
    , ("eval: [2^n]",            check 100 prop_Powers_of_2)
    , ("eval: x@f=f=f@x",        check 100 prop_Compose)
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
    , ("Geometric series",       check 100 prop_Geometric)
    , ("Labeled trees 1",        check   1 prop_Labeled_trees_1)
    , ("Labeled trees 2",        check   1 prop_Labeled_trees_2)
    , ("Unlabeled trees",        check   1 prop_Unlabeled_trees)
    , ("Labeled graphs",         check   1 prop_Labeled_graphs)
    , ("Connected labeled graphs", check 1 prop_Connected_labeled_graphs)
    , ("Derivative of geometric",  check 1 prop_Derivative_of_geometric)
    , ("Product rule",           check  50 prop_Product_rule)
    , ("Reciprocal rule",        check  50 prop_Reciprocal_rule)
    , ("Chain rule",             check  50 prop_Chain_rule)
    , ("Fund Thm of Calc",       check 100 prop_Fundamental)
    , ("Integration by parts",   check 100 prop_Integration_by_parts)
    , ("A235802",                check   1 prop_A235802)
    , ("A001710-1",              check   1 prop_A001710_1)
    , ("A001710-2",              check   1 prop_A001710_2)
    , ("A075834-1",              check   1 prop_A075834_1)
    , ("A075834-2",              check   1 prop_A075834_2)
    , ("A003149",                check   1 prop_A003149)
    , ("Fredholm-Rueppel-1",     check   1 prop_Fredholm_Rueppel_1)
    , ("Fredholm-Rueppel-2",     check   1 prop_Fredholm_Rueppel_2)
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
    , ("A053492",                check   1 prop_A053492)
    , ("A066397",                check   1 prop_A066397)
    , ("A088789",                check   1 prop_A088789)
    , ("A049140",                check   1 prop_A049140)
    , ("A008965",                check   1 prop_A008965)
    , ("Determinant",            check 100 prop_Determinant)
    , ("Coefficients",           check   1 prop_Coefficients)
    ]

main =
    forM_ tests $ \(name, chk) -> do
        putStr (name ++ ": ")
        result <- chk
        unless (isSuccess result) exitFailure
