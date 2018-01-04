{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Copyright   : Anders Claesson 2015-2017
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module HOPS.GF
    ( module HOPS.GF.Series
    , module HOPS.GF.Transform
    , module HOPS.Pretty
    , Expr (..)
    , Expr0 (..)
    , Expr1 (..)
    , Expr2 (..)
    , Expr3 (..)
    , PackedExpr (..)
    , Name
    , nameSupply
    , packExpr
    , vars
    , anums
    , insertVar
    , aNumExpr
    , tagExpr
    -- Expand
    , expand
    -- Core
    , Core (..)
    , core
    -- Eval
    , Env (..)
    , emptyEnv
    , evalCoreS
    , evalCore
    -- Parse
    , parseExpr
    , parseExprErr
    ) where

import GHC.TypeLits
import Data.Proxy
import Data.Maybe
import Data.List
import Data.Ratio
import Data.Semigroup
import Data.Aeson (FromJSON (..), ToJSON(..), Value (..))
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8 hiding (take, takeWhile)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Control.Monad
import Control.Monad.Trans.State
import Control.Applicative
import HOPS.Pretty
import HOPS.Utils.Parse
import HOPS.OEIS
import HOPS.GF.Series
import HOPS.GF.Transform
import qualified HOPS.GF.Rats as R
import qualified HOPS.GF.Const as C

-- | A compact `ByteString` representation of a `Prg`.
newtype PackedExpr = PackedExpr ByteString deriving (Eq, Show)

instance ToJSON PackedExpr where
    toJSON (PackedExpr bs) = String (decodeUtf8 bs)

instance FromJSON PackedExpr where
    parseJSON (String s) = pure $ PackedExpr (encodeUtf8 s)
    parseJSON _ = mzero

-- | An environment holds a mapping from A-numbers to series, and a
-- mapping from names to series (assignments).
data Env (n :: Nat) = Env
    { aNumEnv :: Vector (Series n)
    , varEnv  :: Map Name (Series n)
    } deriving Show

type Name = ByteString -- Variable name

type Subs = Name -> Name

data Expr
    = Singleton Expr0
    | ELet Name Expr0
    | ESeq Expr Expr
    deriving (Show, Eq)

data Expr0
    = EAdd Expr0 Expr0
    | ESub Expr0 Expr0
    | Expr1 Expr1
    deriving (Show, Eq)

data Expr1
    = EMul Expr1 Expr1
    | EDiv Expr1 Expr1
    | EBDP Expr1 Expr1
    | EPtMul Expr1 Expr1
    | EPtDiv Expr1 Expr1
    | Expr2 Expr2
    deriving (Show, Eq)

data Expr2
    = ENeg Expr2
    | EPos Expr2
    | EFac Expr3
    | EPow Expr3 Expr3
    | EComp Expr3 Expr3
    | ECoef Expr3 Expr3
    | Expr3 Expr3
    deriving (Show, Eq)

data Expr3
    = EX
    | EDZ
    | EIndet
    | EA Int -- An A-number
    | ETag Int
    | EVar Name
    | ELit Integer
    | EApp Name [Expr0] -- A named transform
    | ESet Name [Expr] -- A named set of expressions
    | ERats R.Rats
    | Expr Expr
    deriving (Show, Eq)

to0 :: Expr -> Expr0
to0 (Singleton e) = e
to0 e = Expr1 (Expr2 (Expr3 (Expr e)))

to1 :: Expr -> Expr1
to1 (Singleton (Expr1 e)) = e
to1 e = Expr2 (Expr3 (Expr e))

from3 :: Expr3 -> Expr
from3 = Singleton . Expr1 . Expr2 . Expr3

instance Num Expr where
    e1 + e2 = Singleton $ EAdd (to0 e1) (to0 e2)
    e1 - e2 = Singleton $ ESub (to0 e1) (to0 e2)
    e1 * e2 = Singleton $ Expr1 $ EMul (to1 e1) (to1 e2)
    fromInteger = from3 . ELit
    abs = from3 . EApp "abs" . pure . to0
    signum = from3 . EApp "sgn" . pure . to0

data Core
    = App !Name ![Core]
    | X
    | A   {-# UNPACK #-} !Int
    | Tag {-# UNPACK #-} !Int
    | Var {-# UNPACK #-} !Name
    | Lit !Rat
    | Rats !R.Core
    | Let {-# UNPACK #-} !Name !Core
    | Seq !Core !Core
    deriving (Show, Eq, Ord)

instance Pretty Core where
    pretty (App f es) = f <> paren (foldl' (<>) "" $ intersperse "," $ map pretty es)
    pretty X = "x"
    pretty (A i) = B.cons 'A' (pad 6 i)
    pretty (Tag i) = "TAG" <> pad 6 i
    pretty (Var s) = s
    pretty (Lit t) = maybe (pretty t) pretty $ maybeInteger t
    pretty (Rats r) = pretty r
    pretty (Let s e) = s <> "=" <> pretty e
    pretty (Seq e e') = pretty e <> ";" <> pretty e'

instance Num Core where
    (+) x y = App "add" [x,y]
    (-) x y = App "sub" [x,y]
    (*) x y = App "mul" [x,y]
    abs x = App "abs" [x]
    signum x = App "sgn" [x]
    fromInteger = Lit . fromInteger

instance Fractional Core where
    fromRational = Lit . fromRational
    (/) x y = App "div" [x,y]

instance Floating Core where
    pi = Lit pi
    exp x = App "exp" [x]
    log x = App "log" [x]
    sin x = App "sin" [x]
    cos x = App "cos" [x]
    asin x = App "arcsin" [x]
    acos x = App "arccos" [x]
    atan x = App "arctan" [x]
    sinh x = App "sinh" [x]
    cosh x = App "cosh" [x]
    asinh x = App "arsinh" [x]
    acosh x = App "arcosh" [x]
    atanh x = App "artanh" [x]

instance ToJSON Expr where
    toJSON = toJSON . decodeUtf8 . pretty

instance FromJSON Expr where
    parseJSON (String t) = fromMaybe mzero (return <$> parseExpr (encodeUtf8 t))
    parseJSON _ = mzero

instance Semigroup Expr where
    p <> q = snd $ rename nameSupply (p2 `ESeq` q2)
      where
        (vs, p1) = normalForm nameSupply p
        (us, p2) = rename vs p1
        ( _, q1) = rename us q
        ELet s _ = lastExpr p2
        q2 = subs [("stdin", s)] q1

instance Pretty Expr where
    pretty (Singleton e) = pretty e
    pretty (ELet s e)    = s <> "=" <> pretty e
    pretty (ESeq e1 e2)  = pretty e1 <> ";" <> pretty e2

instance Pretty Expr0 where
    pretty (EAdd e1 e2) = pretty e1 <> "+" <> pretty e2
    pretty (ESub e1 e2) = pretty e1 <> "-" <> pretty e2
    pretty (Expr1 e)    = pretty e

instance Pretty Expr1 where
    pretty (EMul e1 e2)   = pretty e1 <> "*"  <> pretty e2
    pretty (EDiv e1 e2)   = pretty e1 <> "/"  <> pretty e2
    pretty (EBDP e1 e2)   = pretty e1 <> "<>" <> pretty e2
    pretty (EPtMul e1 e2) = pretty e1 <> ".*" <> pretty e2
    pretty (EPtDiv e1 e2) = pretty e1 <> "./" <> pretty e2
    pretty (Expr2 e)      = pretty e

instance Pretty Expr2 where
    pretty (ENeg e) = "-" <> pretty e
    pretty (EPos e) = pretty e
    pretty (EFac e) = pretty e <> "!"
    pretty (EPow e1 e2) = pretty e1 <> "^" <> pretty e2
    pretty (EComp e1 e2) = pretty e1 <> "@" <> pretty e2
    pretty (ECoef e1 e2) = pretty e1 <> "?" <> pretty e2
    pretty (Expr3 e) = pretty e

instance Pretty Expr3 where
    pretty EX = "x"
    pretty EDZ = "DZ"
    pretty EIndet = "Indet"
    pretty (EA i) = B.cons 'A' (pad 6 i)
    pretty (ETag i) = "TAG" <> pad 6 i
    pretty (EVar s) = s
    pretty (ELit t) = pretty t
    pretty (EApp s es) = s <> paren (foldl' (<>) "" $ intersperse "," $ map pretty es)
    pretty (ESet s es) = s <> paren (foldl' (<>) "" $ intersperse "," $ map pretty es)
    pretty (ERats r) = pretty r
    pretty (Expr e) = paren $ pretty e

-- | @pad d n@ packs the integer @n@ into a `ByteString` padding with
-- \'0\' on the right to achieve length @d@.
--
-- > pad 6 123 = "000123"
--
pad :: Int -> Int -> ByteString
pad d n = B.replicate (d - B.length s) '0' <> s where s = B.pack (show n)

-- | A compact representation of an `Expr` as a wrapped `ByteString`.
packExpr :: Expr -> PackedExpr
packExpr = PackedExpr . pretty

-- | The list of variables in a program.
vars :: Core -> [Name]
vars = nub . varsCore

-- | The list of A-numbers in a program.
anums :: Core -> [Int]
anums = nub . anumsCore

subsExpr :: Subs -> Expr -> Expr
subsExpr f (Singleton e) = Singleton (subsExpr0 f e)
subsExpr f (ELet s e)    = ELet (f s) (subsExpr0 f e)
subsExpr f (ESeq e1 e2)  = ESeq (subsExpr f e1) (subsExpr f e2)

subsExpr0 :: Subs -> Expr0 -> Expr0
subsExpr0 f (EAdd e1 e2) = EAdd (subsExpr0 f e1) (subsExpr0 f e2)
subsExpr0 f (ESub e1 e2) = ESub (subsExpr0 f e1) (subsExpr0 f e2)
subsExpr0 f (Expr1 e)    = Expr1 (subsExpr1 f e)

subsExpr1 :: Subs -> Expr1 -> Expr1
subsExpr1 f (EMul e1 e2)   = EMul (subsExpr1 f e1) (subsExpr1 f e2)
subsExpr1 f (EDiv e1 e2)   = EDiv (subsExpr1 f e1) (subsExpr1 f e2)
subsExpr1 f (EBDP e1 e2)   = EBDP (subsExpr1 f e1) (subsExpr1 f e2)
subsExpr1 f (EPtMul e1 e2) = EPtMul (subsExpr1 f e1) (subsExpr1 f e2)
subsExpr1 f (EPtDiv e1 e2) = EPtDiv (subsExpr1 f e1) (subsExpr1 f e2)
subsExpr1 f (Expr2 e)      = Expr2 (subsExpr2 f e)

subsExpr2 :: Subs -> Expr2 -> Expr2
subsExpr2 f (ENeg e) = ENeg (subsExpr2 f e)
subsExpr2 f (EPos e) = EPos (subsExpr2 f e)
subsExpr2 f (EFac e) = EFac (subsExpr3 f e)
subsExpr2 f (EPow e1 e2) = EPow (subsExpr3 f e1) (subsExpr3 f e2)
subsExpr2 f (EComp e1 e2) = EComp (subsExpr3 f e1) (subsExpr3 f e2)
subsExpr2 f (ECoef e1 e2) = ECoef (subsExpr3 f e1) (subsExpr3 f e2)
subsExpr2 f (Expr3 e) = Expr3 (subsExpr3 f e)

subsExpr3 :: Subs -> Expr3 -> Expr3
subsExpr3 f (EVar s) = EVar (f s)
subsExpr3 f (EApp s es) = EApp s (map (subsExpr0 f) es)
subsExpr3 f (Expr e) = Expr (subsExpr f e)
subsExpr3 _ e = e

subs :: [(Name, Name)] -> Expr -> Expr
subs assoc = subsExpr f
  where
    f k = let d = M.fromList assoc in M.findWithDefault k k d

vars' :: Expr -> [Name]
vars' prog = vars (core prog) \\ ["stdin"]

nameSupply :: [Name]
nameSupply = B.words "f g h p q r s t u v w"
          ++ [ B.pack ('f':show i) | i <- [0::Int ..] ]

lastExpr :: Expr -> Expr
lastExpr (ESeq _ e) = lastExpr e
lastExpr e = e

normalForm :: [Name] -> Expr -> ([Name], Expr)
normalForm vs e = nf e
  where
    nf (Singleton e0) = let u:us = vs \\ vars' e in (us, ELet u e0)
    nf e1@(ELet _ _) = (vs, e1)
    nf (ESeq e1 e2) = let (us, e3) = nf e2 in (us, ESeq e1 e3)

rename :: [Name] -> Expr -> ([Name], Expr)
rename vs p = (names, subs assoc p)
  where
    names = vs \\ map snd assoc
    assoc = zip (vars' p) vs

lookupANum :: Int -> Env n -> Maybe (Series n)
lookupANum i env = aNumEnv env !? (i-1)

lookupVar :: ByteString -> Env n -> Maybe (Series n)
lookupVar v env = M.lookup v (varEnv env)

-- | Insert a variable binding into the given environment.
insertVar :: ByteString -> Series n -> Env n -> Env n
insertVar v f (Env a vs) = Env a (M.insert v f vs)

aNumExpr :: Int -> Expr
aNumExpr m = Singleton $ Expr1 (Expr2 (Expr3 (EA m)))

tagExpr :: Int -> Expr
tagExpr m = Singleton $ Expr1 (Expr2 (Expr3 (ETag m)))

--------------------------------------------------------------------------------
-- Expand phase
--------------------------------------------------------------------------------

polyList :: Int -> Int -> [[Int]]
polyList _ 0 = [[]]
polyList 0 j = [ replicate j 0 ]
polyList d j = [ i:xs | i <- [-d .. d], xs <- polyList (d - abs i) (j-1) ]

polyList1 :: Int -> Int -> [[Int]]
polyList1 d j = [ 1:xs | xs <- polyList (d-1) (j-1) ]

fromList :: [Int] -> Expr3
fromList cs = ERats (lift <$> init cs, R.Constant (lift (last cs)), R.Poly)
  where
    lift = C.Expr1 . C.Expr2 . C.Expr3 . C.ELit . fromIntegral

divide :: Expr3 -> Expr3 -> Expr3
divide e1 e2 = Expr $ Singleton $ Expr1 $ EDiv (Expr2 (Expr3 e1)) (Expr2 (Expr3 e2))

polys :: Int -> [Expr3]
polys d = [ fromList cs | cs <- polyList d d, any (/=0) cs ]

polys1 :: Int -> [Expr3]
polys1 d = [ fromList cs | cs <- polyList1 d d ]

rationals :: Int -> [Expr3]
rationals d = divide <$> polys d <*> polys1 d

-- Calkin-Wilf sequence
cw :: Int -> Rational
cw 0 = 1
cw k = 1 / (2*y - x + 1)
  where
    x = cwStream !! (k-1)
    y = fromInteger (truncate x)

cwStream :: [Rational]
cwStream = map cw [0..]

cwFracs :: Int -> [Rational]
cwFracs n = take (2*(2^n-1)) (cwStream >>= \x -> [-x,x])

fracs :: Int -> [Expr3]
fracs n = [ divide (ELit (numerator r)) (ELit (denominator r)) | r <- cwFracs n ]

lookupSet :: Name -> [Expr] -> [Expr3]
lookupSet "poly" [Singleton (Expr1 (Expr2 (Expr3 (ELit d))))] = polys (fromIntegral d)
lookupSet "poly" _ = error "'poly' expects an integer"
lookupSet "rat" [Singleton (Expr1 (Expr2 (Expr3 (ELit d))))] = rationals (fromIntegral d)
lookupSet "rat" _ = error "'rat' expects an integer"
lookupSet "frac" [Singleton (Expr1 (Expr2 (Expr3 (ELit d))))] = fracs (fromIntegral d)
lookupSet "frac" _ = error "'frac' expects an integer"
lookupSet "oneof" es = Expr <$> es
lookupSet _ _ = undefined

expand :: Expr -> [Expr]
expand = expandExpr

expandExpr :: Expr -> [Expr]
expandExpr (Singleton e) = Singleton <$> expandExpr0 e
expandExpr (ELet s e) = ELet s <$> expandExpr0 e
expandExpr (ESeq e1 e2) = ESeq <$> expandExpr e1 <*> expandExpr e2

expandExpr0 :: Expr0 -> [Expr0]
expandExpr0 (EAdd e1 e2) = EAdd <$> expandExpr0 e1 <*> expandExpr0 e2
expandExpr0 (ESub e1 e2) = ESub <$> expandExpr0 e1 <*> expandExpr0 e2
expandExpr0 (Expr1 e) = Expr1 <$> expandExpr1 e

expandExpr1 :: Expr1 -> [Expr1]
expandExpr1 (EMul e1 e2) = EMul <$> expandExpr1 e1 <*> expandExpr1 e2
expandExpr1 (EDiv e1 e2) = EDiv <$> expandExpr1 e1 <*> expandExpr1 e2
expandExpr1 (EBDP e1 e2) = EBDP <$> expandExpr1 e1 <*> expandExpr1 e2
expandExpr1 (EPtMul e1 e2) = EPtMul <$> expandExpr1 e1 <*> expandExpr1 e2
expandExpr1 (EPtDiv e1 e2) = EPtDiv <$> expandExpr1 e1 <*> expandExpr1 e2
expandExpr1 (Expr2 e) = Expr2 <$> expandExpr2 e

expandExpr2 :: Expr2 -> [Expr2]
expandExpr2 (ENeg e) = ENeg <$> expandExpr2 e
expandExpr2 (EPos e) = EPos <$> expandExpr2 e
expandExpr2 (EFac e) = EFac <$> expandExpr3 e
expandExpr2 (EPow e1 e2) = EPow <$> expandExpr3 e1 <*> expandExpr3 e2
expandExpr2 (EComp e1 e2) = EComp <$> expandExpr3 e1 <*> expandExpr3 e2
expandExpr2 (ECoef e1 e2) = ECoef <$> expandExpr3 e1 <*> expandExpr3 e2
expandExpr2 (Expr3 e) = Expr3 <$> expandExpr3 e

expandExpr3 :: Expr3 -> [Expr3]
expandExpr3 EX = [EX]
expandExpr3 EDZ = [EDZ]
expandExpr3 EIndet = [EIndet]
expandExpr3 (EA i) = [EA i]
expandExpr3 (ETag i) = [ETag i]
expandExpr3 (EVar s) = [EVar s]
expandExpr3 (ESet s es) = lookupSet s es
expandExpr3 (ELit t) = [ELit $ fromInteger t]
expandExpr3 (EApp s es) = EApp s <$> sequence (expandExpr0 <$> es)
expandExpr3 (ERats r) = [ERats r]
expandExpr3 (Expr e) = Expr <$> expandExpr e

--------------------------------------------------------------------------------
-- Core
--------------------------------------------------------------------------------

core :: Expr -> Core
core = coreExpr

coreExpr :: Expr -> Core
coreExpr (Singleton e) = coreExpr0 e
coreExpr (ELet s e) = Let s (coreExpr0 e)
coreExpr (ESeq e1 e2) = Seq (coreExpr e1) (coreExpr e2)

coreExpr0 :: Expr0 -> Core
coreExpr0 (EAdd e1 e2) = App "add" [coreExpr0 e1, coreExpr0 e2]
coreExpr0 (ESub e1 e2) = App "sub" [coreExpr0 e1, coreExpr0 e2]
coreExpr0 (Expr1 e) = coreExpr1 e

coreExpr1 :: Expr1 -> Core
coreExpr1 (EMul e1 e2) = App "mul" [coreExpr1 e1, coreExpr1 e2]
coreExpr1 (EDiv e1 e2) = App "div" [coreExpr1 e1, coreExpr1 e2]
coreExpr1 (EBDP e1 e2) = App "bdp" [coreExpr1 e1, coreExpr1 e2]
coreExpr1 (EPtMul e1 e2) = App "ptmul" [coreExpr1 e1, coreExpr1 e2]
coreExpr1 (EPtDiv e1 e2) = App "ptdiv" [coreExpr1 e1, coreExpr1 e2]
coreExpr1 (Expr2 e) = coreExpr2 e

coreExpr2 :: Expr2 -> Core
coreExpr2 (ENeg e) = App "neg" [coreExpr2 e]
coreExpr2 (EPos e) = coreExpr2 e
coreExpr2 (EFac e) = App "fac" [coreExpr3 e]
coreExpr2 (EPow e1 e2) = App "pow" [coreExpr3 e1, coreExpr3 e2]
coreExpr2 (EComp e1 e2) = App "comp" [coreExpr3 e1, coreExpr3 e2]
coreExpr2 (ECoef e1 e2) = App "coef" [coreExpr3 e1, coreExpr3 e2]
coreExpr2 (Expr3 e) = coreExpr3 e

coreExpr3 :: Expr3 -> Core
coreExpr3 EX = X
coreExpr3 EDZ = Lit DZ
coreExpr3 EIndet = Lit Indet
coreExpr3 (EA i) = A i
coreExpr3 (ETag i) = Tag i
coreExpr3 (EVar s) = Var s
coreExpr3 (ESet _ _) = error "Internal error"
coreExpr3 (ELit t) = Lit $ fromInteger t
coreExpr3 (EApp s es) = App s (map coreExpr0 es)
coreExpr3 (ERats r) = Rats (R.core r)
coreExpr3 (Expr e) = coreExpr e

varsCore :: Core -> [Name]
varsCore (App _ es) = varsCore =<< es
varsCore (Var s) = [s]
varsCore (Seq e1 e2) = varsCore e1 ++ varsCore e2
varsCore (Let s e) = s : varsCore e
varsCore _ = []

anumsCore :: Core -> [Int]
anumsCore (App _ es) = anumsCore =<< es
anumsCore (A i) = [i]
anumsCore (Seq e1 e2) = anumsCore e1 ++ anumsCore e2
anumsCore (Let _ e) = anumsCore e
anumsCore _ = []

--------------------------------------------------------------------------------
-- Eval
--------------------------------------------------------------------------------

emptyEnv :: Env n
emptyEnv = Env V.empty M.empty

evalName :: KnownNat n => Name -> Env n -> [Series n] -> Series n
evalName t env ss =
  case lookupTransform t of
    Nothing -> case ss of
                 [s] -> fromMaybe nil (lookupVar t env) `o` s
                 _   -> nil
    Just (Transform k f) -> if length ss == k then f ss else nil

evalCoreS1 :: KnownNat n => Core -> State (Env n) (Series n)
evalCoreS1 (App f es) = evalName f <$> get <*> mapM evalCoreS1 es
evalCoreS1 X = return $ polynomial (Proxy :: Proxy n) [0,1]
evalCoreS1 (A i) = fromMaybe nil . lookupANum i <$> get
evalCoreS1 (Tag _) = return nil
evalCoreS1 (Var v) = fromMaybe nil . lookupVar v <$> get
evalCoreS1 (Lit c) = return $ polynomial (Proxy :: Proxy n) [c]
evalCoreS1 (Rats r) = return $ R.evalCore r
evalCoreS1 (Let v e) = do
    (f, env) <- runState (evalCoreS1 e) <$> get
    put (insertVar v f env)
    return f
evalCoreS1 (Seq e e') = do
    (_, env) <- runState (evalCoreS1 e) <$> get
    let (f, env') = runState (evalCoreS1 e') env
    put env'
    return f

evalCoreS :: KnownNat n => Core -> State (Env n) (Series n)
evalCoreS c = go 1
  where
    f0 = nil
    go 0 = return f0
    go n = do
      (f, env) <- runState (evalCoreS1 c) <$> get
      put env
      if n == precision f0
         then return f
         else go (n+1)

-- | Evaluate a program in a given environment. E.g.
--
-- >>> evalCore (emptyEnv :: Env 4) [ log (1/(1-X)) ]
-- series (Proxy :: Proxy 4) [Val (0 % 1),Val (1 % 1),Val (1 % 2),Val (1 % 3)]
--
evalCore :: KnownNat n => Env n -> Core -> Series n
evalCore env c = evalState (evalCoreS c) env

--------------------------------------------------------------------------------
-- Parse
--------------------------------------------------------------------------------

assignment :: Parser (ByteString, Expr0)
assignment = (,) <$> var <*> (string "=" >> expr0)

expr :: Parser Expr
expr = chainl1 (uncurry ELet <$> assignment <|> Singleton <$> expr0) (const ESeq <$> string ";")
       <* (string ";" <|> pure "")

expr0 :: Parser Expr0
expr0 = chainl1 (Expr1 <$> expr1) (op <$> oneOf "+ -") <?> "expr0"
  where
    op "+" = EAdd
    op "-" = ESub
    op _   = error "internal error"

expr1 :: Parser Expr1
expr1 = chainl1 (Expr2 <$> expr2) (op <$> oneOf ".* ./ * / <>") <?> "expr1"
  where
    op "*"  = EMul
    op "/"  = EDiv
    op ".*" = EPtMul
    op "./" = EPtDiv
    op "<>" = EBDP
    op _    = error "internal error"

expr2 :: Parser Expr2
expr2
     =  pm <$> oneOf "+ -" <*> expr2
    <|> (expr3 >>= \g ->
                EPow  g <$> (string "^" *> expr3)
            <|> EComp g <$> (string "@" *> expr3)
            <|> ECoef g <$> (string "?" *> expr3)
            <|> pure (EFac g) <* string "!"
            <|> pure (Expr3 g))
    <?> "expr2"
  where
    pm "+" = EPos
    pm "-" = ENeg
    pm _   = error "internal error"

expr3 :: Parser Expr3
expr3
     =  ESet <$> name' <*> parens (expr  `sepBy` char ',')
    <|> EApp <$> name  <*> parens (expr0 `sepBy` char ',')
    <|> ELit <$> decimal
    <|> const EDZ <$> string "DZ"
    <|> const EIndet <$> string "Indet"
    <|> EA   <$> aNumInt
    <|> ETag <$> tag
    <|> EVar <$> var
    <|> const EX <$> string "x"
    <|> ERats <$> R.rats
    <|> Expr <$> parens expr
    <?> "expr3"

reserved :: [Name]
reserved = "x" : transforms

name' :: Parser Name
name' = string "poly" <|> string "rat" <|> string "frac" <|> string "oneof"

name :: Parser Name
name = mappend <$> takeWhile1 isAlpha_ascii
               <*> A.takeWhile (\c -> isAlpha_ascii c || isDigit c || c == '_')

var :: Parser ByteString
var = name >>= \s -> if s `elem` reserved then mzero else return s

-- | Parse an expression
parseExpr :: ByteString -> Maybe Expr
parseExpr = parse_ (expr <* endOfInput) . B.takeWhile (/='#') . B.filter f
  where
    f '\t' = False
    f ' '  = False
    f _    = True

-- | Parse a program and possibly fail with an error.
parseExprErr :: ByteString -> Expr
parseExprErr = fromMaybe (error "error parsing program") . parseExpr
