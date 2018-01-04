{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Copyright   : Anders Claesson 2015-2017
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module Main (main) where

import GHC.TypeLits
import Data.Proxy
import Data.Maybe
import Data.Ratio
import Data.Semigroup
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (decodeStrict', encode)
import Control.Parallel.Strategies
import System.Directory
import System.IO
import HOPS.Entry
import HOPS.OEIS
import HOPS.Options
import HOPS.Config
import HOPS.Download
import HOPS.DB
import HOPS.GF

versionString :: String
versionString = "0.8.3"

seqsURL :: String
seqsURL = "https://oeis.org/stripped.gz"

data Input (n :: Nat)
    = RunPrgs (Env n) [Expr] [Core] [Entry]
    | TagSeqs Int [Sequence]
    | UpdateDBs FilePath FilePath
    | Empty

data Output
    = Entries [Entry]
    | NOP

lines' :: BL.ByteString -> [B.ByteString]
lines' = filter (not . B.null) . map BL.toStrict . BL.lines

readStdin :: IO [B.ByteString]
readStdin = lines' <$> BL.getContents

decodeErr :: B.ByteString -> Entry
decodeErr = fromMaybe (error "error decoding JSON") . decodeStrict'

readEntries :: IO [Entry]
readEntries = map decodeErr <$> readStdin

readPrgs :: Options -> IO ([Expr], [Core])
readPrgs opts = do
    prgs <- concatMap (expand . parseExprErr) <$>
                if script opts == ""
                then return (map B.pack (program opts))
                else lines' <$> BL.readFile (script opts)
    return (prgs, core <$> prgs)

mkEntry :: (ANum, Sequence) -> Entry
mkEntry (ANum a, s) = Entry (aNumExpr a) s []

readDB :: Config -> IO [Entry]
readDB = fmap (map mkEntry . parseStripped . unDB) . readSeqDB

readSeqs :: IO [Sequence]
readSeqs = map (parseIntegerSeq . B.filter (/=' ')) <$> readStdin

readInput :: KnownNat n => Options -> Config -> IO (Input n)
readInput opts cfg
    | version opts = return Empty
    | update opts = return $ UpdateDBs (hopsDir cfg) (seqDBPath cfg)
    | isJust (tagSeqs opts) = TagSeqs (fromJust (tagSeqs opts)) <$> readSeqs
    | otherwise = do
        (prgs, cprgs) <- readPrgs opts
        inp <- if forAll opts
               then readDB cfg
               else if "stdin" `elem` (vars =<< cprgs)
                    then readEntries
                    else return []
        db  <- if null (anums =<< cprgs)
               then return emptyANumDB
               else readANumDB cfg
        return $ RunPrgs (Env db M.empty) prgs cprgs inp

printOutput :: Output -> IO ()
printOutput NOP = return ()
printOutput (Entries es) = mapM_ (BL.putStrLn . encode) es

stdEnv :: KnownNat n => Proxy n -> Env n -> Sequence -> Env n
stdEnv n (Env a v) s = Env a $ M.insert "stdin" (series n (map Val s)) v

evalMany :: KnownNat n => Env n -> [Core] -> [Sequence]
evalMany env = map (rationalPrefix . evalCore env)

runPrgs :: KnownNat n => [Env n] -> [Core] -> [Sequence]
runPrgs envs progs =
    concat ( [ evalMany env progs
             | env <- envs
             ] `using` parBuffer 256 rdeepseq )

hops :: KnownNat n => Options -> Proxy n -> Input n -> IO Output
hops opts n inp =
    case inp of

      UpdateDBs hopsdir sdbPath -> do
          createDirectoryIfMissing False hopsdir
          let msg1 = "Downloading " ++ seqsURL ++ ": "
          putStr msg1 >> hFlush stdout
          download (length msg1) seqsURL sdbPath >> putStrLn ""
          return NOP

      TagSeqs i0 ts ->
          return $ Entries [ Entry (tagExpr i) t [] | (i, t) <- zip [i0 .. ] ts ]

      Empty -> putStrLn ("hops " ++ versionString) >> return NOP

      RunPrgs env prgs cprgs entries ->
          return $ Entries
             [ Entry p f t
             | (p, f, t) <- zip3 ps results (trails ++ repeat [])
             , not (int opts) || all (\r -> denominator r == 1) f
             , minPrec opts == 0 || minPrec opts <= length f
             ]
        where
          (qs, seqs, trails) = unzip3 [ (q,s,t) | Entry q s t <- entries ]
          results = runPrgs envs cprgs
          (ps, envs) = if null qs
                       then (prgs, [env])
                       else ((<>) <$> qs <*> prgs, map (stdEnv n env) seqs)

-- | Main function and entry point for hops.
main :: IO ()
main = do
    c <- getConfig
    t <- getOptions
    let d = fromIntegral (prec t)
    case someNatVal d of
      Nothing -> error $ show d ++ " not a valid prec"
      Just (SomeNat (_ :: Proxy n)) ->
          readInput t c >>= hops t (Proxy :: Proxy n) >>= printOutput
