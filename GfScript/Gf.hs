{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module GfScript.Gf
    ( Expr0 (..)
    , Expr1 (..)
    , Expr2 (..)
    , Expr3 (..)
    , Expr4 (..)
    , Cmd (..)
    , PackedPrg (..)
    , Prg (..)
    , Pretty (..)
    , packPrg
    , vars
    , insertVar
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
import GfScript.Utils
import GfScript.OEIS
import GfScript.Gf.Series
import GfScript.Gf.Transform
import GfScript.Gf.Const (Pretty(..))
import GfScript.Gf.Rats

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
    = Pow (Expr3 a) (Expr2 a)
    | Expr3 (Expr3 a)
    deriving (Show, Eq)

data Expr3 a
    = Neg (Expr3 a)
    | Pos (Expr3 a)
    | Fac (Expr4 a)
    | Tr Name (Expr4 a) -- A named transform
    | Comp (Expr4 a) (Expr4 a)
    | Expr4 (Expr4 a)
    deriving (Show, Eq)

data Expr4 a
    = X
    | A Int -- A-numbers
    | Tag Int
    | Var Name
    | Lit a
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
    pprint (Pow e1 e2) = pprint e1 <> "^" <> pprint e2
    pprint (Expr3 e)   = pprint e

instance (Eq a, Num a, Pretty a) => Pretty (Expr3 a) where
    pprint (Neg e) = "-" <> pprint e
    pprint (Pos e) = pprint e
    pprint (Fac e) = pprint e <> "!"
    pprint (Tr s e) = s <> pprint e
    pprint (Comp e1 e2) = pprint e1 <> "@" <> pprint e2
    pprint (Expr4 e) = pprint e

instance (Eq a, Num a, Pretty a) => Pretty (Expr4 a) where
    pprint X = "x"
    pprint (A i) = B.cons 'A' (pad 6 i)
    pprint (Tag i) = "TAG" <> pad 6 i
    pprint (Var s) = s
    pprint (Lit x) = pprint x
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
varsExpr2 (Pow e1 e2) = varsExpr3 e1 ++ varsExpr2 e2
varsExpr2 (Expr3 e)   = varsExpr3 e

varsExpr3 :: Expr3 a -> [Name]
varsExpr3 (Neg e)  = varsExpr3 e
varsExpr3 (Pos e)  = varsExpr3 e
varsExpr3 (Fac e)  = varsExpr4 e
varsExpr3 (Tr _ e) = varsExpr4 e
varsExpr3 (Comp e1 e2) = varsExpr4 e1 ++ varsExpr4 e2
varsExpr3 (Expr4 e) = varsExpr4 e

varsExpr4 :: Expr4 a -> [Name]
varsExpr4 (A _) = []
varsExpr4 (Tag _) = []
varsExpr4 (Var s) = [s]
varsExpr4 (Expr0 e) = varsExpr0 e
varsExpr4 _ = []

varsCmd :: Cmd a -> [Name]
varsCmd (Expr e) = varsExpr0 e
varsCmd (Asgmt s e) = s : varsExpr0 e

varsPrg :: Prg a -> [Name]
varsPrg = nub . (>>= varsCmd) . commands

-- | The list of variables in a program.
vars :: Prg a -> [Name]
vars = varsPrg

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
subsExpr2 f (Pow e1 e2) = Pow (subsExpr3 f e1) (subsExpr2 f e2)
subsExpr2 f (Expr3 e)   = Expr3 (subsExpr3 f e)

subsExpr3 :: Subs -> Expr3 a -> Expr3 a
subsExpr3 f (Neg e)  = Neg (subsExpr3 f e)
subsExpr3 f (Pos e)  = Pos (subsExpr3 f e)
subsExpr3 f (Fac e)  = Fac (subsExpr4 f e)
subsExpr3 f (Tr s e) = Tr s (subsExpr4 f e)
subsExpr3 f (Comp e1 e2) = Comp (subsExpr4 f e1) (subsExpr4 f e2)
subsExpr3 f (Expr4 e) = Expr4 (subsExpr4 f e)

subsExpr4 :: Subs -> Expr4 a -> Expr4 a
subsExpr4 _ X = X
subsExpr4 _ (A i) = A i
subsExpr4 _ (Tag i) = Tag i
subsExpr4 f (Var s) = Var (f s)
subsExpr4 _ (Lit x) = Lit x
subsExpr4 _ (Rats r) = Rats r
subsExpr4 f (Expr0 e) = Expr0 (subsExpr0 f e)

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
evalExpr2 env (Pow u e) = evalExpr3 env u ** evalExpr2 env e
evalExpr2 env (Expr3 u) = evalExpr3 env u

evalExpr3 :: KnownNat n => Env n -> Expr3 Integer -> Series n
evalExpr3 env (Neg u)    = negate (evalExpr3 env u)
evalExpr3 env (Pos u)    = evalExpr3 env u
evalExpr3 env (Fac u)    = fac $ evalExpr4 env u
evalExpr3 env (Comp g h) = evalExpr4 env g `o` evalExpr4 env h
evalExpr3 env (Expr4 g)  = evalExpr4 env g
evalExpr3 env (Tr t g)   =
    let g'  = evalExpr4 env g
    in case lookupTransform t of
         Just tr -> tr g'
         Nothing -> fromMaybe nil (lookupVar t env) `o` g'

evalExpr4 :: KnownNat n => Env n -> Expr4 Integer -> Series n
evalExpr4 _    X        = polynomial (Proxy :: Proxy n) [0,1]
evalExpr4 env (A i)     = fromMaybe nil (lookupANum i env)
evalExpr4 _   (Tag _)   = nil
evalExpr4 env (Var v)   = fromMaybe nil (lookupVar v env)
evalExpr4 _   (Lit c)   = polynomial (Proxy :: Proxy n) [Val (toRational c)]
evalExpr4 _   (Rats r)  = evalRats r
evalExpr4 env (Expr0 e) = evalExpr0 env e

evalCmd :: KnownNat n => Env n -> Cmd Integer -> (Env n, Series n)
evalCmd env (Expr e) = (env, evalExpr0 env e)
evalCmd env (Asgmt v e) = (insertVar v f env, f) where f = evalExpr0 env e

evalPrgNext :: KnownNat n => Prg Integer -> (Env n, Series n) -> (Env n, Series n)
evalPrgNext (Prg cs) (env, f) = foldl' (\(ev, _) c -> evalCmd ev c) (env, f) cs

nil :: KnownNat n => Series n
nil = series (Proxy :: Proxy n) []

-- | Evaluate a program in a given environment.
evalPrg :: KnownNat n => Env n -> Prg Integer -> (Env n, Series n)
evalPrg env p = snd $ fromMaybe t (find hasConverged (zip trail trail'))
  where
    t = ((env, nil), (env, nil))
    trail  = iterate (evalPrgNext p) (env, nil)
    trail' = drop 1 trail
    hasConverged ((_,a), (_,b)) = a == b

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
expr2 p = do
    u <- expr3 p
    Pow u <$> (string "^" *> expr2 p) <|> pure (Expr3 u) <?> "expr2"

expr3 :: (Eq a, Num a) => Parser a -> Parser (Expr3 a)
expr3 p
     =  pm <$> oneOf "+ -" <*> expr3 p
    <|> Tr <$> name <*> expr4 p
    <|> (expr4 p >>= \g ->
            Comp g <$> (string "@" *> expr4 p)
            <|> pure (Fac g) <* string "!"
            <|> pure (Expr4 g))
    <?> "expr3"
  where
    pm "+" = Pos
    pm "-" = Neg
    pm _   = error "internal error"

expr4 :: (Eq a, Num a) => Parser a -> Parser (Expr4 a)
expr4 p
     =  Lit     <$> p
    <|> A       <$> aNumInt
    <|> Tag     <$> tag
    <|> Var     <$> var
    <|> const X <$> string "x"
    <|> Rats    <$> rats p
    <|> Expr0   <$> parens (expr0 p)
    <?> "expr4"

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
parsePrg = parse_ prg . B.takeWhile (/='#') . B.filter (/=' ')

-- | Parse a program and possibly fail with an error.
parsePrgErr :: ByteString -> Prg Integer
parsePrgErr = fromMaybe (error "error parsing program") . parsePrg
