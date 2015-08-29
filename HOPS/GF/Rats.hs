{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
--
-- Expressions defining sequences of rational numbers.

module HOPS.GF.Rats
    ( Linear (..)
    , Fun
    , Term (..)
    , Rats
    , evalRats
    , rats
    ) where

import Prelude as P
import GHC.TypeLits
import Data.Monoid
import Data.Proxy
import Data.Maybe
import Data.Foldable (find)
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import HOPS.GF.Const
import HOPS.GF.Series

data Linear a = a :+ a deriving (Show, Eq)

type Fun a = Expr (Linear a)

data Term a
    = Ellipsis
    | Constant (Expr a)
    | Fun (Fun a)
    deriving (Show, Eq)

-- | An expression defining a sequence.
type Rats a = ([Expr a], Term a)

instance (Eq a, Num a, Pretty a) => Pretty (Linear a) where
    pprint (0 :+ b) | b == 0 = pprint b
    pprint (0 :+ 1) = "n"
    pprint (0 :+ b) = pprint b <> "*n"
    pprint (a :+ 0) = pprint a
    pprint (a :+ b) = pprint a <> "+" <> pprint b <> "*n"

instance (Eq a, Num a, Pretty a) => Pretty (Term a) where
    pprint Ellipsis = "..."
    pprint (Constant e) = pprint e
    pprint (Fun f) = pprint f

instance (Eq a, Num a, Pretty a) => Pretty (Rats a) where
    pprint (cs, t) = "{" <> B.intercalate "," (map pprint cs ++ [pprint t]) <> "}"

--------------------------------------------------------------------------------
-- Eval
--------------------------------------------------------------------------------

evalLinear :: Num a => a -> Linear a -> a
evalLinear n (a :+ b) = a + b * n

-- | Evaluate an expression obtaining the series defined by that
-- expression.
evalRats :: KnownNat n => Rats Integer -> Series n
evalRats (as, finalTerm) =
    let cs = map evalExpr as
    in case finalTerm of
        Ellipsis   -> series (Proxy :: Proxy n) (newtonExtension cs)
        Constant e -> series (Proxy :: Proxy n) (cs ++ [ evalExpr e ])
        t -> let e = case t of
                       Fun d    -> d
                       _        -> error "internal error"
             in series (Proxy :: Proxy n)
                    (cs ++ [ evalExpr (evalLinear (fromIntegral n) <$> e)
                           | n <- [length cs ..]
                           ])

choose :: (Integral b, Fractional a) => a -> b -> a
choose x k = p / q
  where
    p = product [ x - fromIntegral i | i <- [0..k-1] ]
    q = fromIntegral (product [1 .. toInteger k])

newtonExtension :: Fractional a => [a] -> [a]
newtonExtension cs = map (f . fromIntegral) [0::Int ..]
  where
    f n = sum (zipWith (\k c -> c * (n `choose` k)) [0::Int ..] coefficients)
    coefficients = map head (newtonTriangle cs)
    differences xs = zipWith (-) (drop 1 xs) xs
    newtonTriangle = P.takeWhile (not . null) . iterate differences

--------------------------------------------------------------------------------
-- Parse
--------------------------------------------------------------------------------

linear :: (Eq a, Num a) => Parser a -> Parser (Linear a)
linear p = const (0 :+ 1) <$> string "n" <|> (:+ 0) <$> p

fun :: (Eq a, Num a) => Parser a -> Parser (Fun a)
fun p = expr (linear p) <?> "function"

funTerm :: (Eq a, Num a) => Parser a -> Parser (Term a)
funTerm p = Fun <$> fun p

ellipsis :: Parser (Term a)
ellipsis = const Ellipsis <$> string "..."

term :: (Eq a, Num a) => Parser a -> Parser (Term a)
term p = ellipsis <|> funTerm p

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` string ","

decompose :: [a] -> Maybe ([a], a)
decompose [] = Nothing
decompose xs = Just (init xs, last xs)

toConstant :: (Eq a, Num a) => Term a -> Term a
toConstant (Fun e) | isConstant e = Constant ((\(a :+ _) -> a) <$> e)
toConstant f = f

isConstant :: (Eq a, Num a) => Expr (Linear a) -> Bool
isConstant e = isNothing (find (\(_ :+ b) -> b /= 0) e)

-- | Parser for `Rats`.
rats :: (Eq a, Num a) => Parser a -> Parser (Rats a)
rats p = toRats <$> (string "{" *> commaSep (term p) <* string "}")
  where
    coerce (Constant e) = e
    coerce (Fun _) = error "unexpected 'n'"
    coerce _ = error "unexpected ellipsis"
    toRats rs = fromMaybe (error "at least one term expected") $ do
        (ts, t) <- decompose (toConstant <$> rs)
        return (coerce <$> ts, t)
