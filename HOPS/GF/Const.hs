{-# LANGUAGE OverloadedStrings #-}

-- |
-- Copyright   : Anders Claesson 2015, 2016
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module HOPS.GF.Const
    ( Expr
    , Expr0 (..)
    , Expr1 (..)
    , Expr2 (..)
    , Expr3 (..)
    , Core (..)
    , indet
    , zero
    , core
    , simplify
    , isConstant
    , evalExpr
    , evalCore
    , expr
    ) where

import Data.Monoid
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import HOPS.GF.Series
import HOPS.Utils.Parse
import HOPS.Pretty

-- | An expression for a constant. Supports addition, subtraction,
-- multiplication, division, exponentials and factorials.
type Expr = Expr0

data Expr0
    = EAdd Expr0 Expr0
    | ESub Expr0 Expr0
    | Expr1 Expr1
    deriving (Show, Eq)

data Expr1
    = EMul Expr1 Expr1
    | EDiv Expr1 Expr1
    | Expr2 Expr2
    deriving (Show, Eq)

data Expr2
    = ENeg Expr2
    | EPos Expr2
    | EFac Expr3
    | EPow Expr3 Expr3
    | Expr3 Expr3
    deriving (Show, Eq)

data Expr3
    = ELit Integer
    | EN
    | EDZ
    | EIndet
    | Expr0 Expr0
    deriving (Show, Eq)

data Fun1 = Neg | Fac deriving (Show, Eq, Ord)

instance Pretty Fun1 where
    pretty Neg = "-"
    pretty Fac = "!"

data Fun2 = Add | Sub | Mul | Div | Pow deriving (Show, Eq, Ord)

instance Pretty Fun2 where
    pretty Add = "+"
    pretty Sub = "-"
    pretty Mul = "*"
    pretty Div = "/"
    pretty Pow = "^"

data Core
    = App1 Fun1 Core
    | App2 Fun2 Core Core
    | Binom Int        -- binomial(N,k)
    | Lit Rat
    | N
    deriving (Show, Eq, Ord)

instance Pretty Core where
    pretty (App1 f e) = pretty f <> paren (pretty e)
    pretty (App2 op e1 e2) = paren (pretty e1 <> pretty op <> pretty e2)
    pretty (Binom k) = "binom" <> paren (pretty N <> "," <> pretty k)
    pretty (Lit t) = maybe (pretty t) pretty $ maybeInteger t
    pretty N = "n"

instance Num Core where
    (+) = App2 Add
    (-) = App2 Sub
    (*) = App2 Mul
    fromInteger i = Lit (fromInteger i)
    abs = undefined
    signum = undefined

instance Pretty Expr0 where
    pretty (EAdd e1 e2) = pretty e1 <> "+" <> pretty e2
    pretty (ESub e1 e2) = pretty e1 <> "-" <> pretty e2
    pretty (Expr1 e)    = pretty e

instance Pretty Expr1 where
    pretty (EMul e1 e2) = pretty e1 <> "*"  <> pretty e2
    pretty (EDiv e1 e2) = pretty e1 <> "/"  <> pretty e2
    pretty (Expr2 e)    = pretty e

instance Pretty Expr2 where
    pretty (ENeg e) = "-" <> pretty e
    pretty (EPos e) = pretty e
    pretty (EFac e) = pretty e <> "!"
    pretty (EPow e k) = pretty e <> "^" <> pretty k
    pretty (Expr3 e)  = pretty e

instance Pretty Expr3 where
    pretty (ELit x) = pretty x
    pretty EN = "n"
    pretty EDZ = "DZ"
    pretty EIndet = "Indet"
    pretty (Expr0 e) = paren (pretty e)

indet :: Core
indet = Lit Indet

zero :: Core
zero = Lit (Val 0)

--------------------------------------------------------------------------------
-- Core
--------------------------------------------------------------------------------

core :: Expr -> Core
core = simplify . coreExpr0

simplifyLit :: Core -> Core
simplifyLit (App1 _  (Lit DZ)) = Lit DZ
simplifyLit (App1 Neg (Lit i)) = Lit (-i)
simplifyLit (App1 Fac (Lit i)) = Lit (factorial i)
simplifyLit (App2 _ (Lit DZ) _) = Lit DZ
simplifyLit (App2 _ _ (Lit DZ)) = Lit DZ
simplifyLit (App2 Add (Lit i) (Lit j)) = Lit (i + j)
simplifyLit (App2 Add (Lit 0) e      ) = e
simplifyLit (App2 Add e       (Lit 0)) = e
simplifyLit (App2 Sub (Lit i) (Lit j)) = Lit (i - j)
simplifyLit (App2 Sub (Lit 0) e      ) = App1 Neg e
simplifyLit (App2 Sub e       (Lit 0)) = e
simplifyLit (App2 Mul (Lit i) (Lit j)) = Lit (i * j)
simplifyLit (App2 Mul (Lit 1) e      ) = e
simplifyLit (App2 Mul e       (Lit 1)) = e
simplifyLit (App2 Div e       (Lit 1)) = e
simplifyLit (App2 Div (Lit i) (Lit j)) = Lit (i / j)
simplifyLit e = e

simplify :: Core -> Core
simplify (App1 f e) = simplifyLit $ App1 f (simplify e)
simplify (App2 f e1 e2) = simplifyLit $ App2 f (simplify e1) (simplify e2)
simplify e = e

coreExpr0 :: Expr0 -> Core
coreExpr0 (EAdd e1 e2) = App2 Add (coreExpr0 e1) (coreExpr0 e2)
coreExpr0 (ESub e1 e2) = App2 Sub (coreExpr0 e1) (coreExpr0 e2)
coreExpr0 (Expr1 e)    = coreExpr1 e

coreExpr1 :: Expr1 -> Core
coreExpr1 (EMul e1 e2) = App2 Mul (coreExpr1 e1) (coreExpr1 e2)
coreExpr1 (EDiv e1 e2) = App2 Div (coreExpr1 e1) (coreExpr1 e2)
coreExpr1 (Expr2 e)    = coreExpr2 e

coreExpr2 :: Expr2 -> Core
coreExpr2 (ENeg e)     = App1 Neg (coreExpr2 e)
coreExpr2 (EPos e)     = coreExpr2 e
coreExpr2 (EFac e)     = App1 Fac (coreExpr3 e)
coreExpr2 (EPow e1 e2) = App2 Pow (coreExpr3 e1) (coreExpr3 e2)
coreExpr2 (Expr3 e)    = coreExpr3 e

coreExpr3 :: Expr3 -> Core
coreExpr3 (ELit c)  = fromInteger c
coreExpr3 EN        = N
coreExpr3 EDZ       = Lit DZ
coreExpr3 EIndet    = Lit Indet
coreExpr3 (Expr0 e) = coreExpr0 e

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

isConstant :: Expr -> Bool
isConstant = isC . core
  where
    isC (App1 _ e) = isC e
    isC (App2 _ e1 e2) = isC e1 && isC e2
    isC N = False
    isC _ = True

--------------------------------------------------------------------------------
-- Eval
--------------------------------------------------------------------------------

evalFun1 :: Fun1 -> Rat -> Rat
evalFun1 Neg = negate
evalFun1 Fac = factorial

evalFun2 :: Fun2 -> Rat -> Rat -> Rat
evalFun2 Add = (+)
evalFun2 Sub = (-)
evalFun2 Mul = (*)
evalFun2 Div = (/)
evalFun2 Pow = (!^!)

-- | The value of the given (core) expression.
evalCore :: Int -> Core -> Rat
evalCore n (App1 f e) = evalFun1 f (evalCore n e)
evalCore n (App2 f e1 e2) = evalFun2 f (evalCore n e1) (evalCore n e2)
evalCore n N = fromIntegral n
evalCore _ (Lit c) = c
evalCore n (Binom k) = fromIntegral $ binomial n k

-- | The value of the given expression.
evalExpr :: Int -> Expr -> Rat
evalExpr n = evalCore n . core

--------------------------------------------------------------------------------
-- Parse
--------------------------------------------------------------------------------

-- | Parser for an `Expr`.
expr :: Parser Expr
expr = expr0

expr0 :: Parser Expr0
expr0 = chainl1 (Expr1 <$> expr1) (op0 <$> oneOf "+ -") <?> "expr0"
  where
    op0 "+" = EAdd
    op0 "-" = ESub
    op0 _   = error "internal error"

expr1 :: Parser Expr1
expr1 = chainl1 (Expr2 <$> expr2) (op1 <$> oneOf "* /") <?> "expr1"
  where
    op1 "*" = EMul
    op1 "/" = EDiv
    op1 _   = error "internal error"

expr2 :: Parser Expr2
expr2
     =  op3 <$> oneOf "+ -" <*> expr2
    <|> do { u <- expr3
           ; choice [ return (EFac u) <* string "!"
                    , EPow u <$> (string "^" *> expr3)
                    , return (Expr3 u)
                    ]
           }
    <?> "expr2"
  where
    op3 "+" = EPos
    op3 "-" = ENeg
    op3 _   = error "internal error"

expr3 :: Parser Expr3
expr3
    = string "n" *> return EN
   <|> const EDZ <$> string "DZ"
   <|> const EIndet <$> string "Indet"
   <|> ELit <$> decimal
   <|> Expr0 <$> parens expr0 <?> "expr3"
