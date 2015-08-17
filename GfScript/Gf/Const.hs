{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module GfScript.Gf.Const
    ( Expr
    , Expr0 (..)
    , Expr1 (..)
    , Expr2 (..)
    , Expr3 (..)
    , Expr4 (..)
    , Pretty (..)
    , evalExpr
    , expr
    ) where

import Data.Monoid
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable
#endif
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import GfScript.Gf.Series
import GfScript.Utils

-- | Similar to the `Show` class, but uses `ByteString`s rather than
-- `String`s.
class Pretty a where
    pprint :: a -> ByteString

-- | An expression for a constant. Supports addition, subtraction,
-- multiplication, division, exponentials and factorials.
type Expr = Expr0

data Expr0 a
    = Add (Expr0 a) (Expr0 a)
    | Sub (Expr0 a) (Expr0 a)
    | Expr1 (Expr1 a)
    deriving (Show, Eq, Functor, Foldable)

data Expr1 a
    = Mul (Expr1 a) (Expr1 a)
    | Div (Expr1 a) (Expr1 a)
    | Expr2 (Expr2 a)
    deriving (Show, Eq, Functor, Foldable)

data Expr2 a
    = Pow (Expr3 a) (Expr2 a)
    | Expr3 (Expr3 a)
    deriving (Show, Eq, Functor, Foldable)

data Expr3 a
    = Neg (Expr3 a)
    | Pos (Expr3 a)
    | Fac (Expr4 a)
    | Expr4 (Expr4 a)
    deriving (Show, Eq, Functor, Foldable)

data Expr4 a
    = Lit a
    | Expr0 (Expr0 a)
    deriving (Show, Eq, Functor, Foldable)

instance Pretty Integer where
    pprint = B.pack . show

instance Pretty a => Pretty (Expr0 a) where
    pprint (Add e1 e2) = pprint e1 <> "+" <> pprint e2
    pprint (Sub e1 e2) = pprint e1 <> "-" <> pprint e2
    pprint (Expr1 e)   = pprint e

instance Pretty a => Pretty (Expr1 a) where
    pprint (Mul e1 e2) = pprint e1 <> "*"  <> pprint e2
    pprint (Div e1 e2) = pprint e1 <> "/"  <> pprint e2
    pprint (Expr2 e)   = pprint e

instance Pretty a => Pretty (Expr2 a) where
    pprint (Pow e k) = pprint e <> "^" <> pprint k
    pprint (Expr3 e) = pprint e

instance Pretty a => Pretty (Expr3 a) where
    pprint (Neg e) = "-" <> pprint e
    pprint (Pos e) = pprint e
    pprint (Fac e) = pprint e <> "!"
    pprint (Expr4 e) = pprint e

instance Pretty a => Pretty (Expr4 a) where
    pprint (Lit x)   = pprint x
    pprint (Expr0 e) = paren $ pprint e

paren :: ByteString -> ByteString
paren s = "(" <> s <> ")"

--------------------------------------------------------------------------------
-- Eval
--------------------------------------------------------------------------------

-- | The value of the given expression.
evalExpr :: Expr Integer -> Rat
evalExpr = evalExpr0

evalExpr0 :: Expr0 Integer -> Rat
evalExpr0 (Add t e) = evalExpr0 t + evalExpr0 e
evalExpr0 (Sub t e) = evalExpr0 t - evalExpr0 e
evalExpr0 (Expr1 t) = evalExpr1 t

evalExpr1 :: Expr1 Integer -> Rat
evalExpr1 (Mul r t) = evalExpr1 r * evalExpr1 t
evalExpr1 (Div r t) = evalExpr1 r / evalExpr1 t
evalExpr1 (Expr2 r) = evalExpr2 r

evalExpr2 :: Expr2 Integer -> Rat
evalExpr2 (Pow u g) = evalExpr3 u !^! evalExpr2 g
evalExpr2 (Expr3 u) = evalExpr3 u

evalExpr3 :: Expr3 Integer -> Rat
evalExpr3 (Neg u)   = negate (evalExpr3 u)
evalExpr3 (Pos u)   = evalExpr3 u
evalExpr3 (Fac u)   = factorial (evalExpr4 u)
evalExpr3 (Expr4 g) = evalExpr4 g

evalExpr4 :: Expr4 Integer -> Rat
evalExpr4 (Lit c)   = Val (toRational c)
evalExpr4 (Expr0 e) = evalExpr0 e

--------------------------------------------------------------------------------
-- Parse
--------------------------------------------------------------------------------

-- | Parser for an `Expr`.
expr :: Parser a -> Parser (Expr a)
expr = expr0

expr0 :: Parser a -> Parser (Expr0 a)
expr0 p = chainl1 (Expr1 <$> expr1 p) (op0 <$> oneOf "+ -") <?> "expr0"
  where
    op0 "+" = Add
    op0 "-" = Sub
    op0 _   = error "internal error"

expr1 :: Parser a -> Parser (Expr1 a)
expr1 p = chainl1 (Expr2 <$> expr2 p) (op1 <$> oneOf "* /") <?> "expr1"
  where
    op1 "*" = Mul
    op1 "/" = Div
    op1 _   = error "internal error"

expr2 :: Parser a -> Parser (Expr2 a)
expr2 p =
    do { u <- expr3 p
       ; choice [ Pow u <$> (string "^" *> expr2 p <* skipSpace)
                , return (Expr3 u)
                ]
       } <?> "expr2"

expr3 :: Parser a -> Parser (Expr3 a)
expr3 p
     =  op3 <$> oneOf "+ -" <*> expr3 p
    <|> Fac <$> (expr4 p <* string "!")
    <|> Expr4 <$> expr4 p
    <?> "expr3"
  where
    op3 "+" = Pos
    op3 "-" = Neg
    op3 _   = error "internal error"

expr4 :: Parser a -> Parser (Expr4 a)
expr4 p = Lit <$> p <|> Expr0 <$> parens (expr0 p) <?> "expr4"
