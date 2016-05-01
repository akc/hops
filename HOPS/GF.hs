{-# LANGUAGE OverloadedStrings #-}
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
    , CorePrg
    , Pretty (..)
    , Name
    , packPrg
    , vars
    , anums
    , insertVar
    , aNumPrg
    , tagPrg
    -- Core
    , core
    -- Eval
    , Env (..)
    , evalCorePrg
    , evalCorePrgs
    -- Parse
    , parsePrg
    , parsePrgErr
    ) where

import GHC.TypeLits
import Data.Proxy
import Data.Maybe
import Data.List
import Data.Monoid
import Data.Aeson (FromJSON (..), ToJSON(..), Value (..))
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Vector (Vector, (!?))
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
import HOPS.Utils
import HOPS.OEIS
import HOPS.GF.Series
import HOPS.GF.Transform
import qualified HOPS.GF.Rats as R

-- | A compact `ByteString` representation of a `Prg`.
newtype PackedPrg = PPrg ByteString deriving (Eq, Show)

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

type Name = ByteString -- Variable name

type Subs = Name -> Name

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
    | Expr3 Expr3
    deriving (Show, Eq)

data Expr3
    = EX
    | EA Int -- An A-number
    | ETag Int
    | EVar Name
    | ELit Integer
    | Tr Name Expr3 -- A named transform
    | ERats R.Rats
    | Expr0 Expr0
    deriving (Show, Eq)

data Cmd                -- A command is
    = Expr Expr0        -- an expression, or
    | Asgmt Name Expr0  -- an assignment
    deriving (Show, Eq)

data Fun1 = Neg | Fac | Tr1 Name deriving (Show, Eq)

data Fun2
    = Add | Sub
    | Mul | Div
    | BDP | Pow | Comp | PtMul | PtDiv
    deriving (Show, Eq)

data Core
    = App1 !Fun1 !Core
    | App2 !Fun2 !Core !Core
    | X
    | A   {-# UNPACK #-} !Int
    | Tag {-# UNPACK #-} !Int
    | Var {-# UNPACK #-} !Name
    | Lit !Integer
    | Rats !R.Core
    | Let {-# UNPACK #-} !Name !Core
    deriving (Show)

type CorePrg = [Core]

-- | A program is a list of commands, where a command is either a power
-- series expression or an assignment.
newtype Prg = Prg { commands :: [Cmd] } deriving (Show, Eq)

instance ToJSON Prg where
    toJSON = toJSON . decodeUtf8 . pretty

instance FromJSON Prg where
    parseJSON (String t) = fromMaybe mzero (return <$> parsePrg (encodeUtf8 t))
    parseJSON _ = mzero

instance Monoid Prg where
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
    pretty (Expr3 e) = pretty e

instance Pretty Expr3 where
    pretty EX = "x"
    pretty (EA i) = B.cons 'A' (pad 6 i)
    pretty (ETag i) = "TAG" <> pad 6 i
    pretty (EVar s) = s
    pretty (ELit t) = pretty t
    pretty (Tr s e) = s <> pretty e
    pretty (ERats r) = pretty r
    pretty (Expr0 e) = paren $ pretty e

instance Pretty Cmd where
    pretty (Expr e) = pretty e
    pretty (Asgmt s e) = s <> "=" <> pretty e

instance Pretty Prg where
    pretty = B.intercalate ";" . map pretty . commands

-- | A compact representation of a `Prg` as a wrapped `ByteString`.
packPrg :: Prg -> PackedPrg
packPrg = PPrg . pretty

-- | The list of variables in a program.
vars :: CorePrg -> [Name]
vars = varsCorePrg

-- | The list of A-numbers in a program.
anums :: CorePrg -> [Int]
anums = anumsCorePrg

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
subsExpr2 f (Expr3 e) = Expr3 (subsExpr3 f e)

subsExpr3 :: Subs -> Expr3 -> Expr3
subsExpr3 f (EVar s) = EVar (f s)
subsExpr3 f (Tr s e) = Tr s (subsExpr3 f e)
subsExpr3 f (Expr0 e) = Expr0 (subsExpr0 f e)
subsExpr3 _ e = e

subsCmd :: Subs -> Cmd -> Cmd
subsCmd f (Expr e) = Expr (subsExpr0 f e)
subsCmd f (Asgmt s e) = Asgmt (f s) (subsExpr0 f e)

subsPrg :: Subs -> Prg -> Prg
subsPrg f = Prg . map (subsCmd f) . commands

subs :: [(Name, Name)] -> Prg -> Prg
subs assoc = subsPrg f
  where
    f k = let d = M.fromList assoc in M.findWithDefault k k d

vars' :: Prg -> [Name]
vars' prog = vars (core prog) \\ ["stdin"]

nameSupply :: [Name]
nameSupply = B.words "f g h p q r s t u v w"
          ++ [ B.pack ('f':show i) | i <- [0::Int ..] ]

nfEnd :: [Name] -> Prg -> ([Name], Prg)
nfEnd vs prog@(Prg cmds) = (ws, Prg cmds')
  where
    (ws, cmds') = nfEnd' cmds
    nfEnd' []       = (vs, [])
    nfEnd' [Expr e] = let u:us = vs \\ vars' prog in (us, [Asgmt u e])
    nfEnd' [a]      = (vs, [a])
    nfEnd' (c:cs)   = let (us, cs') = nfEnd' cs in (us, c:cs')

rename :: [Name] -> Prg -> ([Name], Prg)
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

aNumPrg :: Int -> Prg
aNumPrg m = Prg [Expr (Expr1 (Expr2 (Expr3 (EA m))))]

tagPrg :: Int -> Prg
tagPrg m = Prg [Expr (Expr1 (Expr2 (Expr3 (ETag m))))]

--------------------------------------------------------------------------------
-- Core
--------------------------------------------------------------------------------

core :: Prg -> CorePrg
core = map coreCmd . commands

coreCmd :: Cmd -> Core
coreCmd (Expr e) = coreExpr0 e
coreCmd (Asgmt s e) = Let s (coreExpr0 e)

coreExpr0 :: Expr0 -> Core
coreExpr0 (EAdd e1 e2) = App2 Add (coreExpr0 e1) (coreExpr0 e2)
coreExpr0 (ESub e1 e2) = App2 Sub (coreExpr0 e1) (coreExpr0 e2)
coreExpr0 (Expr1 e) = coreExpr1 e

coreExpr1 :: Expr1 -> Core
coreExpr1 (EMul e1 e2) = App2 Mul (coreExpr1 e1) (coreExpr1 e2)
coreExpr1 (EDiv e1 e2) = App2 Div (coreExpr1 e1) (coreExpr1 e2)
coreExpr1 (EBDP e1 e2) = App2 BDP (coreExpr1 e1) (coreExpr1 e2)
coreExpr1 (EPtMul e1 e2) = App2 PtMul (coreExpr1 e1) (coreExpr1 e2)
coreExpr1 (EPtDiv e1 e2) = App2 PtDiv (coreExpr1 e1) (coreExpr1 e2)
coreExpr1 (Expr2 e) = coreExpr2 e

coreExpr2 :: Expr2 -> Core
coreExpr2 (ENeg e) = App1 Neg (coreExpr2 e)
coreExpr2 (EPos e) = coreExpr2 e
coreExpr2 (EFac e) = App1 Fac (coreExpr3 e)
coreExpr2 (EPow e1 e2) = App2 Pow (coreExpr3 e1) (coreExpr3 e2)
coreExpr2 (EComp e1 e2) = App2 Comp (coreExpr3 e1) (coreExpr3 e2)
coreExpr2 (Expr3 e) = coreExpr3 e

coreExpr3 :: Expr3 -> Core
coreExpr3 EX = X
coreExpr3 (EA i) = A i
coreExpr3 (ETag i) = Tag i
coreExpr3 (EVar s) = Var s
coreExpr3 (ELit t) = Lit t
coreExpr3 (Tr s e) = App1 (Tr1 s) (coreExpr3 e)
coreExpr3 (ERats r) = Rats (R.core r)
coreExpr3 (Expr0 e) = coreExpr0 e

varsCorePrg :: CorePrg -> [Name]
varsCorePrg = nub . (>>= varsCore)

varsCore :: Core -> [Name]
varsCore (App1 _ e) = varsCore e
varsCore (App2 _ e1 e2) = varsCore e1 ++ varsCore e2
varsCore (Var s) = [s]
varsCore (Let s e) = s : varsCore e
varsCore _ = []

anumsCorePrg :: CorePrg -> [Int]
anumsCorePrg = nub . (>>= anumsCore)

anumsCore :: Core -> [Int]
anumsCore (App1 _ e) = anumsCore e
anumsCore (App2 _ e1 e2) = anumsCore e1 ++ anumsCore e2
anumsCore (A i) = [i]
anumsCore (Let _ e) = anumsCore e
anumsCore _ = []

--------------------------------------------------------------------------------
-- Eval
--------------------------------------------------------------------------------

evalFun1 :: KnownNat n => Fun1 -> Env n -> Series n -> Series n
evalFun1 Neg     _   = negate
evalFun1 Fac     _   = fac
evalFun1 (Tr1 t) env =
    fromMaybe (fromMaybe nil (lookupVar t env) `o`) (lookupTransform t)

evalFun2 :: KnownNat n => Fun2 -> Series n -> Series n -> Series n
evalFun2 Add = (+)
evalFun2 Sub = (-)
evalFun2 Mul = (*)
evalFun2 Div = (/)
evalFun2 BDP = blackDiamond
evalFun2 PtMul = (.*)
evalFun2 PtDiv = (./)
evalFun2 Pow = (**)
evalFun2 Comp = o

evalCore :: KnownNat n => Core -> State (Env n) (Series n)
evalCore (App1 f e) = evalFun1 f <$> get <*> evalCore e
evalCore (App2 f e1 e2) = evalFun2 f <$> evalCore e1 <*> evalCore e2
evalCore X = return $ polynomial (Proxy :: Proxy n) [0,1]
evalCore (A i) = fromMaybe nil . lookupANum i <$> get
evalCore (Tag _) = return nil
evalCore (Var v) = fromMaybe nil . lookupVar v <$> get
evalCore (Lit c) = return $ polynomial (Proxy :: Proxy n) [Val (toRational c)]
evalCore (Rats r) = return $ R.evalCore r
evalCore (Let v e) = do
    (f, env) <- runState (evalCore e) <$> get
    put (insertVar v f env)
    return f

evalCorePrgNext :: KnownNat n => CorePrg -> (Series n, Env n) -> (Series n, Env n)
evalCorePrgNext prog (env, f) =
    foldl' (\(_, ev) c -> runState (evalCore c) ev) (env, f) prog
{-# INLINE evalCorePrgNext #-}

nil :: KnownNat n => Series n
nil = series (Proxy :: Proxy n) []

-- | Evaluate a program in a given environment.
evalCorePrg :: KnownNat n => Env n -> CorePrg -> Series n
evalCorePrg env prog = fst (trail !! precision f0)
  where
    f0 = nil
    trail = iterate (evalCorePrgNext prog) (f0, env)

-- | Evaluate a list of programs in a given environment.
evalCorePrgs :: KnownNat n => Env n -> [CorePrg] -> [Series n]
evalCorePrgs env = map (evalCorePrg env)

--------------------------------------------------------------------------------
-- Parse
--------------------------------------------------------------------------------

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
            <|> pure (EFac g) <* string "!"
            <|> pure (Expr3 g))
    <?> "expr2"
  where
    pm "+" = EPos
    pm "-" = ENeg
    pm _   = error "internal error"

expr3 :: Parser Expr3
expr3
     =  ELit     <$> decimal
    <|> EA       <$> aNumInt
    <|> ETag     <$> tag
    <|> Tr       <$> name <*> expr3
    <|> EVar     <$> var
    <|> const EX <$> string "x"
    <|> ERats    <$> R.rats
    <|> Expr0    <$> parens expr0
    <?> "expr3"

assignment :: Parser (ByteString, Expr0)
assignment = (,) <$> var <*> (string "=" >> expr0)

cmd :: Parser Cmd
cmd = uncurry Asgmt <$> assignment <|> Expr <$> expr0

reserved :: [Name]
reserved = "x" : transforms

name :: Parser Name
name = mappend <$> takeWhile1 isAlpha_ascii
               <*> A.takeWhile (\c -> isAlpha_ascii c || isDigit c || c == '_')

var :: Parser ByteString
var = name >>= \s -> if s `elem` reserved then mzero else return s

prg :: Parser Prg
prg = Prg <$> cmd `sepBy'` string ";"
          <*  (string ";" <|> pure "")
          <*  endOfInput

-- | Parse a program.
parsePrg :: ByteString -> Maybe Prg
parsePrg = parse_ prg . B.takeWhile (/='#') . B.filter f
  where
    f '\t' = False
    f ' '  = False
    f _    = True

-- | Parse a program and possibly fail with an error.
parsePrgErr :: ByteString -> Prg
parsePrgErr = fromMaybe (error "error parsing program") . parsePrg
