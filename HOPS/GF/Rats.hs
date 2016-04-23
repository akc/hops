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
    ( Term (..)
    , Rats
    , Core
    , core
    , evalCore
    , rats
    ) where

import Prelude as P
import GHC.TypeLits
import Data.Monoid
import Data.Proxy
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import HOPS.Pretty
import qualified HOPS.GF.Const as C
import HOPS.GF.Series

data Term
    = Ellipsis
    | Constant C.Expr
    | Fun C.Expr
    deriving (Show, Eq)

-- | An expression defining a sequence.
type Rats = ([C.Expr], Term)

instance Pretty Term where
    pretty Ellipsis = "..."
    pretty (Constant e) = pretty e
    pretty (Fun f) = pretty f

instance Pretty Rats where
    pretty (cs, t) = "{" <> B.intercalate "," (map pretty cs ++ [pretty t]) <> "}"

--------------------------------------------------------------------------------
-- Core
--------------------------------------------------------------------------------

type Core = ([C.Core], C.Core)

core :: Rats -> Core
core (es, t) =
    let cs = map C.core es
    in case t of
      Ellipsis   -> ( []              , newtonPoly cs )
      Constant e -> ( cs ++ [C.core e], C.indet       )
      Fun e      -> ( cs              , C.core e      )

newtonPoly :: [C.Core] -> C.Core
newtonPoly es =
    C.simplify $ sum (zipWith (\k c -> (C.Lit c * C.Binom k)) [0::Int ..] cs)
  where
    cs = map head (newtonTriangle (zipWith C.evalCore [0..] es))
    newtonTriangle = P.takeWhile (not . null) . iterate diffs
    diffs xs = zipWith (-) (drop 1 xs) xs

--------------------------------------------------------------------------------
-- Eval
--------------------------------------------------------------------------------

evalCore :: KnownNat n => Core -> Series n
evalCore (es, t) =
    series (Proxy :: Proxy n) $ zipWith C.evalCore [0..] (es ++ repeat t)

--------------------------------------------------------------------------------
-- Parse
--------------------------------------------------------------------------------

term :: Parser Term
term = const Ellipsis <$> string "..." <|> Fun <$> C.expr

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` string ","

decompose :: [a] -> Maybe ([a], a)
decompose [] = Nothing
decompose xs = Just (init xs, last xs)

toConstant :: Term -> Term
toConstant (Fun e) | C.isConstant e = Constant e
toConstant f = f

-- | Parser for `Rats`.
rats :: Parser Rats
rats = toRats <$> (string "{" *> commaSep term <* string "}")
  where
    coerce (Constant e) = e
    coerce (Fun _) = error "unexpected 'n'"
    coerce Ellipsis = error "unexpected ellipsis"
    toRats rs = fromMaybe (error "at least one term expected") $ do
        (ts, t) <- decompose (toConstant <$> rs)
        return (coerce <$> ts, t)
