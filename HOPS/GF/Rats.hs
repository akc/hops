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
    , SequenceType (..)
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
import HOPS.Utils.Parse

data Term
    = Ellipsis
    | Constant C.Expr
    | Fun C.Expr
    deriving (Show, Eq)

data SequenceType = Poly | Ser deriving (Show, Eq, Ord)

-- | An expression defining a sequence.
type Rats = ([C.Expr], Term, SequenceType)

instance Pretty Term where
    pretty Ellipsis = "..."
    pretty (Constant e) = pretty e
    pretty (Fun f) = pretty f

instance Pretty Rats where
    pretty (cs, t, stype) =
        let (bra, ket) = sequencetype ("[", "]") ("{", "}") stype
        in bra <> B.intercalate "," (map pretty cs ++ [pretty t]) <> ket

--------------------------------------------------------------------------------
-- Core
--------------------------------------------------------------------------------

type Core = ([C.Core], C.Core, SequenceType)

instance Pretty Core where
    pretty (cs, c, Poly) = bracket $ B.intercalate "," $ map pretty (cs ++ [c])
    pretty (cs, c, Ser ) = curly   $ B.intercalate "," $ map pretty (cs ++ [c])

sequencetype :: a -> a -> SequenceType -> a
sequencetype x _ Poly = x
sequencetype _ y Ser  = y

core :: Rats -> Core
core (es, t, stype) =
    let cs = map C.core es
        fill = sequencetype C.zero C.indet stype
    in case t of
        Ellipsis   -> ( []              , newtonPoly cs, stype )
        Constant e -> ( cs ++ [C.core e], fill         , stype )
        Fun e      -> ( cs              , C.core e     , stype )

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
evalCore (es, t, stype) =
    sequencetype polynomial series stype (Proxy :: Proxy n) $
        zipWith C.evalCore [0..] (es ++ repeat t)

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

sequenceOfTerms :: Parser ([Term], SequenceType)
sequenceOfTerms = do
    bra <- string "{" <|> string "["
    ts <- commaSep term
    let (ket, stype) = if bra == "{" then ("}", Ser) else ("]", Poly)
    string ket
    return (ts, stype)

-- | Parser for `Rats`.
rats :: Parser Rats
rats = toRats <$> sequenceOfTerms
  where
    coerce (Constant e) = e
    coerce (Fun _) = error "unexpected 'n'"
    coerce Ellipsis = error "unexpected ellipsis"
    toRats (rs, stype) = fromMaybe (error "at least one term expected") $ do
        (ts, t) <- decompose (toConstant <$> rs)
        return (coerce <$> ts, t, stype)
