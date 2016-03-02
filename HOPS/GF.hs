{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Copyright   : Anders Claesson 2015, 2016
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module HOPS.GF
    ( Expr0 (..)
    , Expr1 (..)
    , Expr2 (..)
    , Expr3 (..)
    , Cmd (..)
    , PackedPrg (..)
    , Prg (..)
    , Pretty (..)
    , packPrg
    , vars
    , anums
    , insertVar
    , aNumPrg
    , tagPrg
    -- Eval
    , Env (..)
    , evalPrg
    , evalPrgs
    -- Parse
    , parsePrg
    , parsePrgErr
    ) where

import GHC.Generics (Generic)
import GHC.TypeLits
import Data.Proxy
import Data.Maybe
import Data.List
import Data.Monoid
import Data.Aeson
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Vector (Vector, (!?))
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8 hiding (take, takeWhile)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Control.Monad
import Control.Applicative
import HOPS.Utils
import HOPS.OEIS
import HOPS.GF.Series
import HOPS.GF.Transform
import HOPS.GF.Const (Pretty(..))
import HOPS.GF.Rats

-- | A compact `ByteString` representation of a `Prg`.
newtype PackedPrg = PPrg ByteString deriving (Eq, Show, Generic)

instance ToJSON PackedPrg where
    toJSON (PPrg bs) = String (decodeUtf8 bs)

instance FromJSON PackedPrg where
    parseJSON (String s) = pure $ PPrg (encodeUtf8 s)
    parseJSON _ = mzero

-- | An environment holds a mapping from A-numbers to series, and a
-- mapping from names to series (assignments).
data Env (n :: Nat) = Env
    { aNumEnv :: Vector (Series n)
    , varEnv  :: Map Name (Series n)
    }

type Subs = Name -> Name

data Expr0 a
    = Add (Expr0 a) (Expr0 a)
    | Sub (Expr0 a) (Expr0 a)
    | Expr1 (Expr1 a)
    deriving (Show, Eq)

data Expr1 a
    = Mul (Expr1 a) (Expr1 a)
    | Div (Expr1 a) (Expr1 a)
    | PtMul (Expr1 a) (Expr1 a)
    | PtDiv (Expr1 a) (Expr1 a)
    | Expr2 (Expr2 a)
    deriving (Show, Eq)

data Expr2 a
    = Neg (Expr2 a)
    | Pos (Expr2 a)
    | Fac (Expr3 a)
    | Pow (Expr3 a) (Expr3 a)
    | Comp (Expr3 a) (Expr3 a)
    | Expr3 (Expr3 a)
    deriving (Show, Eq)

data Expr3 a
    = X
    | A Int -- An A-number
    | Tag Int
    | Var Name
    | Lit a
    | Tr Name (Expr3 a) -- A named transform
    | Rats (Rats a)
    | Expr0 (Expr0 a)
    deriving (Show, Eq)

data Cmd a                  -- A command is
    = Expr (Expr0 a)        -- an expression, or
    | Asgmt Name (Expr0 a)  -- an assignment
    deriving (Show, Eq)

-- | A program is a list of commands, where a command is either a power
-- series expression or an assignment.
newtype Prg a = Prg { commands :: [Cmd a] } deriving (Show, Eq)

instance ToJSON (Prg Integer) where
    toJSON = toJSON . decodeUtf8 . pprint

instance FromJSON (Prg Integer) where
    parseJSON (String t) = fromMaybe mzero (return <$> parsePrg (encodeUtf8 t))
    parseJSON _ = mzero

instance Monoid (Prg a) where
    mempty = Prg []
    mappend (Prg []) q = q
    mappend p (Prg []) = p
    mappend p q = snd $ rename nameSupply (Prg $ commands p'' ++ commands q'')
      where
        (vs, p' ) = nfEnd nameSupply p
        (us, p'') = rename vs p'
        ( _, q' ) = rename us q
        Asgmt ident _ = last (commands p'')
        q'' = subs [("stdin", ident)] q'

instance (Eq a, Num a, Pretty a) => Pretty (Expr0 a) where
    pprint (Add e1 e2) = pprint e1 <> "+" <> pprint e2
    pprint (Sub e1 e2) = pprint e1 <> "-" <> pprint e2
    pprint (Expr1 e)   = pprint e

instance (Eq a, Num a, Pretty a) => Pretty (Expr1 a) where
    pprint (Mul e1 e2)   = pprint e1 <> "*"  <> pprint e2
    pprint (Div e1 e2)   = pprint e1 <> "/"  <> pprint e2
    pprint (PtMul e1 e2) = pprint e1 <> ".*" <> pprint e2
    pprint (PtDiv e1 e2) = pprint e1 <> "./" <> pprint e2
    pprint (Expr2 e)     = pprint e

instance (Eq a, Num a, Pretty a) => Pretty (Expr2 a) where
    pprint (Neg e) = "-" <> pprint e
    pprint (Pos e) = pprint e
    pprint (Fac e) = pprint e <> "!"
    pprint (Pow e1 e2) = pprint e1 <> "^" <> pprint e2
    pprint (Comp e1 e2) = pprint e1 <> "@" <> pprint e2
    pprint (Expr3 e) = pprint e

instance (Eq a, Num a, Pretty a) => Pretty (Expr3 a) where
    pprint X = "x"
    pprint (A i) = B.cons 'A' (pad 6 i)
    pprint (Tag i) = "TAG" <> pad 6 i
    pprint (Var s) = s
    pprint (Lit x) = pprint x
    pprint (Tr s e) = s <> pprint e
    pprint (Rats r) = pprint r
    pprint (Expr0 e) = paren $ pprint e

instance (Eq a, Num a, Pretty a) => Pretty (Cmd a) where
    pprint (Expr e) = pprint e
    pprint (Asgmt s e) = s <> "=" <> pprint e

instance (Eq a, Num a, Pretty a) => Pretty (Prg a) where
    pprint = B.intercalate ";" . map pprint . commands

-- | A compact representation of a `Prg` as a wrapped `ByteString`.
packPrg :: Prg Integer -> PackedPrg
packPrg = PPrg . pprint

paren :: ByteString -> ByteString
paren s = "(" <> s <> ")"

varsExpr0 :: Expr0 a -> [Name]
varsExpr0 (Add e1 e2) = varsExpr0 e1 ++ varsExpr0 e2
varsExpr0 (Sub e1 e2) = varsExpr0 e1 ++ varsExpr0 e2
varsExpr0 (Expr1 e)   = varsExpr1 e

varsExpr1 :: Expr1 a -> [Name]
varsExpr1 (Mul e1 e2)   = varsExpr1 e1 ++ varsExpr1 e2
varsExpr1 (Div e1 e2)   = varsExpr1 e1 ++ varsExpr1 e2
varsExpr1 (PtMul e1 e2) = varsExpr1 e1 ++ varsExpr1 e2
varsExpr1 (PtDiv e1 e2) = varsExpr1 e1 ++ varsExpr1 e2
varsExpr1 (Expr2 e)     = varsExpr2 e

varsExpr2 :: Expr2 a -> [Name]
varsExpr2 (Neg e)  = varsExpr2 e
varsExpr2 (Pos e)  = varsExpr2 e
varsExpr2 (Fac e)  = varsExpr3 e
varsExpr2 (Pow e1 e2) = varsExpr3 e1 ++ varsExpr3 e2
varsExpr2 (Comp e1 e2) = varsExpr3 e1 ++ varsExpr3 e2
varsExpr2 (Expr3 e) = varsExpr3 e

varsExpr3 :: Expr3 a -> [Name]
varsExpr3 (A _) = []
varsExpr3 (Tag _) = []
varsExpr3 (Var s) = [s]
varsExpr3 (Tr _ e) = varsExpr3 e
varsExpr3 (Expr0 e) = varsExpr0 e
varsExpr3 _ = []

varsCmd :: Cmd a -> [Name]
varsCmd (Expr e) = varsExpr0 e
varsCmd (Asgmt s e) = s : varsExpr0 e

varsPrg :: Prg a -> [Name]
varsPrg = nub . (>>= varsCmd) . commands

-- | The list of variables in a program.
vars :: Prg a -> [Name]
vars = varsPrg

anumsExpr0 :: Expr0 a -> [Int]
anumsExpr0 (Add e1 e2) = anumsExpr0 e1 ++ anumsExpr0 e2
anumsExpr0 (Sub e1 e2) = anumsExpr0 e1 ++ anumsExpr0 e2
anumsExpr0 (Expr1 e)   = anumsExpr1 e

anumsExpr1 :: Expr1 a -> [Int]
anumsExpr1 (Mul e1 e2)   = anumsExpr1 e1 ++ anumsExpr1 e2
anumsExpr1 (Div e1 e2)   = anumsExpr1 e1 ++ anumsExpr1 e2
anumsExpr1 (PtMul e1 e2) = anumsExpr1 e1 ++ anumsExpr1 e2
anumsExpr1 (PtDiv e1 e2) = anumsExpr1 e1 ++ anumsExpr1 e2
anumsExpr1 (Expr2 e)     = anumsExpr2 e

anumsExpr2 :: Expr2 a -> [Int]
anumsExpr2 (Neg e)  = anumsExpr2 e
anumsExpr2 (Pos e)  = anumsExpr2 e
anumsExpr2 (Fac e)  = anumsExpr3 e
anumsExpr2 (Pow e1 e2) = anumsExpr3 e1 ++ anumsExpr3 e2
anumsExpr2 (Comp e1 e2) = anumsExpr3 e1 ++ anumsExpr3 e2
anumsExpr2 (Expr3 e) = anumsExpr3 e

anumsExpr3 :: Expr3 a -> [Int]
anumsExpr3 (A i) = [i]
anumsExpr3 (Tag _) = []
anumsExpr3 (Var _) = []
anumsExpr3 (Tr _ e) = anumsExpr3 e
anumsExpr3 (Expr0 e) = anumsExpr0 e
anumsExpr3 _ = []

anumsCmd :: Cmd a -> [Int]
anumsCmd (Expr e) = anumsExpr0 e
anumsCmd (Asgmt _ e) = anumsExpr0 e

anumsPrg :: Prg a -> [Int]
anumsPrg = nub . (>>= anumsCmd) . commands

-- | The list of A-numbers in a program.
anums :: Prg a -> [Int]
anums = anumsPrg

subsExpr0 :: Subs -> Expr0 a -> Expr0 a
subsExpr0 f (Add e1 e2) = Add (subsExpr0 f e1) (subsExpr0 f e2)
subsExpr0 f (Sub e1 e2) = Sub (subsExpr0 f e1) (subsExpr0 f e2)
subsExpr0 f (Expr1 e)   = Expr1 (subsExpr1 f e)

subsExpr1 :: Subs -> Expr1 a -> Expr1 a
subsExpr1 f (Mul e1 e2)   = Mul (subsExpr1 f e1) (subsExpr1 f e2)
subsExpr1 f (Div e1 e2)   = Div (subsExpr1 f e1) (subsExpr1 f e2)
subsExpr1 f (PtMul e1 e2) = PtMul (subsExpr1 f e1) (subsExpr1 f e2)
subsExpr1 f (PtDiv e1 e2) = PtDiv (subsExpr1 f e1) (subsExpr1 f e2)
subsExpr1 f (Expr2 e)     = Expr2 (subsExpr2 f e)

subsExpr2 :: Subs -> Expr2 a -> Expr2 a
subsExpr2 f (Neg e)  = Neg (subsExpr2 f e)
subsExpr2 f (Pos e)  = Pos (subsExpr2 f e)
subsExpr2 f (Fac e)  = Fac (subsExpr3 f e)
subsExpr2 f (Pow e1 e2) = Pow (subsExpr3 f e1) (subsExpr3 f e2)
subsExpr2 f (Comp e1 e2) = Comp (subsExpr3 f e1) (subsExpr3 f e2)
subsExpr2 f (Expr3 e) = Expr3 (subsExpr3 f e)

subsExpr3 :: Subs -> Expr3 a -> Expr3 a
subsExpr3 _ X = X
subsExpr3 _ (A i) = A i
subsExpr3 _ (Tag i) = Tag i
subsExpr3 f (Var s) = Var (f s)
subsExpr3 _ (Lit x) = Lit x
subsExpr3 f (Tr s e) = Tr s (subsExpr3 f e)
subsExpr3 _ (Rats r) = Rats r
subsExpr3 f (Expr0 e) = Expr0 (subsExpr0 f e)

subsCmd :: Subs -> Cmd a -> Cmd a
subsCmd f (Expr e) = Expr (subsExpr0 f e)
subsCmd f (Asgmt s e) = Asgmt (f s) (subsExpr0 f e)

subsPrg :: Subs -> Prg a -> Prg a
subsPrg f = Prg . map (subsCmd f) . commands

subs :: [(Name, Name)] -> Prg a -> Prg a
subs assoc = subsPrg f
  where
    f k = let d = M.fromList assoc in M.findWithDefault k k d

vars' :: Prg a -> [Name]
vars' prog = vars prog \\ ["stdin"]

nameSupply :: [Name]
nameSupply = B.words "f g h p q r s t u v w"
          ++ [ B.pack ('f':show i) | i <- [0::Int ..] ]

nfEnd :: [Name] -> Prg a -> ([Name], Prg a)
nfEnd vs prog@(Prg cmds) = (ws, Prg cmds')
  where
    (ws, cmds') = nfEnd' cmds
    nfEnd' []       = (vs, [])
    nfEnd' [Expr e] = let u:us = vs \\ vars' prog in (us, [Asgmt u e])
    nfEnd' [a]      = (vs, [a])
    nfEnd' (c:cs)   = let (us, cs') = nfEnd' cs in (us, c:cs')

rename :: [Name] -> Prg a -> ([Name], Prg a)
rename vs p = (names, subs assoc p)
  where
    names = vs \\ map snd assoc
    assoc = zip (vars' p) vs

lookupANum :: KnownNat n => Int -> Env n -> Maybe (Series n)
lookupANum i env = aNumEnv env !? (i-1)

lookupVar :: KnownNat n => ByteString -> Env n -> Maybe (Series n)
lookupVar v env = M.lookup v (varEnv env)

-- | Insert a variable binding into the given environment.
insertVar :: KnownNat n => ByteString -> Series n -> Env n -> Env n
insertVar v f (Env a vs) = Env a (M.insert v f vs)

aNumPrg :: Int -> Prg Integer
aNumPrg m = Prg [Expr (Expr1 (Expr2 (Expr3 (A m))))]

tagPrg :: Int -> Prg Integer
tagPrg m = Prg [Expr (Expr1 (Expr2 (Expr3 (Tag m))))]

--------------------------------------------------------------------------------
-- Eval
--------------------------------------------------------------------------------

evalExpr0 :: KnownNat n => Env n -> Expr0 Integer -> Series n
evalExpr0 env (Add t e) = evalExpr0 env t + evalExpr0 env e
evalExpr0 env (Sub t e) = evalExpr0 env t - evalExpr0 env e
evalExpr0 env (Expr1 t) = evalExpr1 env t

evalExpr1 :: KnownNat n => Env n -> Expr1 Integer -> Series n
evalExpr1 env (Mul r t)   = evalExpr1 env r *  evalExpr1 env t
evalExpr1 env (Div r t)   = evalExpr1 env r /  evalExpr1 env t
evalExpr1 env (PtMul r t) = evalExpr1 env r .* evalExpr1 env t
evalExpr1 env (PtDiv r t) = evalExpr1 env r ./ evalExpr1 env t
evalExpr1 env (Expr2 r)   = evalExpr2 env r

evalExpr2 :: KnownNat n => Env n -> Expr2 Integer -> Series n
evalExpr2 env (Neg u)    = negate (evalExpr2 env u)
evalExpr2 env (Pos u)    = evalExpr2 env u
evalExpr2 env (Fac u)    = fac $ evalExpr3 env u
evalExpr2 env (Pow u e)  = evalExpr3 env u ** evalExpr3 env e
evalExpr2 env (Comp g h) = evalExpr3 env g `o` evalExpr3 env h
evalExpr2 env (Expr3 g)  = evalExpr3 env g

evalExpr3 :: KnownNat n => Env n -> Expr3 Integer -> Series n
evalExpr3 _    X        = polynomial (Proxy :: Proxy n) [0,1]
evalExpr3 env (A i)     = fromMaybe nil (lookupANum i env)
evalExpr3 _   (Tag _)   = nil
evalExpr3 env (Var v)   = fromMaybe nil (lookupVar v env)
evalExpr3 _   (Lit c)   = polynomial (Proxy :: Proxy n) [Val (toRational c)]
evalExpr3 _   (Rats r)  = evalRats r
evalExpr3 env (Expr0 e) = evalExpr0 env e
evalExpr3 env (Tr t g)   =
    let g' = evalExpr3 env g
    in case lookupTransform t of
         Just tr -> tr g'
         Nothing -> fromMaybe nil (lookupVar t env) `o` g'

evalCmd :: KnownNat n => Env n -> Cmd Integer -> (Env n, Series n)
evalCmd env (Expr e) = (env, evalExpr0 env e)
evalCmd env (Asgmt v e) = (insertVar v f env, f) where f = evalExpr0 env e

evalPrgNext :: KnownNat n => Prg Integer -> (Env n, Series n) -> (Env n, Series n)
evalPrgNext (Prg cs) (env, f) = foldl' (\(ev, _) c -> evalCmd ev c) (env, f) cs

nil :: KnownNat n => Series n
nil = series (Proxy :: Proxy n) []

-- | Evaluate a program in a given environment.
evalPrg :: KnownNat n => Env n -> Prg Integer -> (Env n, Series n)
evalPrg env p = trail !! precision f0
  where
    f0 = nil
    trail = iterate (evalPrgNext p) (env, f0)

-- | Evaluate a list of programs in a given environment.
evalPrgs :: KnownNat n => Env n -> [Prg Integer] -> [(Env n, Series n)]
evalPrgs env = map (evalPrg env)

--------------------------------------------------------------------------------
-- Parse
--------------------------------------------------------------------------------

expr0 :: (Eq a, Num a) => Parser a -> Parser (Expr0 a)
expr0 p = chainl1 (Expr1 <$> expr1 p) (op <$> oneOf "+ -") <?> "expr0"
  where
    op "+" = Add
    op "-" = Sub
    op _   = error "internal error"

expr1 :: (Eq a, Num a) => Parser a -> Parser (Expr1 a)
expr1 p = chainl1 (Expr2 <$> expr2 p) (op <$> oneOf ".* ./ * /") <?> "expr1"
  where
    op "*"  = Mul
    op "/"  = Div
    op ".*" = PtMul
    op "./" = PtDiv
    op _    = error "internal error"

expr2 :: (Eq a, Num a) => Parser a -> Parser (Expr2 a)
expr2 p
     =  pm <$> oneOf "+ -" <*> expr2 p
    <|> (expr3 p >>= \g ->
                Pow g <$> (string "^" *> expr3 p)
            <|> Comp g <$> (string "@" *> expr3 p)
            <|> pure (Fac g) <* string "!"
            <|> pure (Expr3 g))
    <?> "expr2"
  where
    pm "+" = Pos
    pm "-" = Neg
    pm _   = error "internal error"

expr3 :: (Eq a, Num a) => Parser a -> Parser (Expr3 a)
expr3 p
     =  Lit     <$> p
    <|> A       <$> aNumInt
    <|> Tag     <$> tag
    <|> Tr      <$> name <*> expr3 p
    <|> Var     <$> var
    <|> const X <$> string "x"
    <|> Rats    <$> rats p
    <|> Expr0   <$> parens (expr0 p)
    <?> "expr3"

assignment :: (Eq a, Num a) => Parser a -> Parser (ByteString, Expr0 a)
assignment p = (,) <$> var <*> (string "=" >> expr0 p)

cmd :: (Eq a, Num a) => Parser a -> Parser (Cmd a)
cmd p = uncurry Asgmt <$> assignment p <|> Expr <$> expr0 p

reserved :: [Name]
reserved = "x" : transforms

name :: Parser Name
name = mappend <$> takeWhile1 isAlpha_ascii
               <*> A.takeWhile (\c -> isAlpha_ascii c || isDigit c || c == '_')

var :: Parser ByteString
var = name >>= \s -> if s `elem` reserved then mzero else return s

prg :: Parser (Prg Integer)
prg = Prg <$> cmd decimal `sepBy'` string ";"
          <*  (string ";" <|> pure "")
          <*  endOfInput

-- | Parse a program.
parsePrg :: ByteString -> Maybe (Prg Integer)
parsePrg = parse_ prg . B.takeWhile (/='#') . B.filter f
  where
    f '\t' = False
    f ' '  = False
    f _    = True

-- | Parse a program and possibly fail with an error.
parsePrgErr :: ByteString -> Prg Integer
parsePrgErr = fromMaybe (error "error parsing program") . parsePrg
