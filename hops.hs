{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Copyright   : Anders Claesson 2015, 2016
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module Main (main) where

import GHC.TypeLits
import Data.Proxy
import Data.Maybe
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
versionString = "0.7.1"

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
    prgs <- map parseExprErr <$>
                if script opts == ""
                then return (map B.pack (program opts))
                else lines' <$> BL.readFile (script opts)
    return (prgs, map core prgs)

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

evalCoreMany :: KnownNat n => Env n -> [Core] -> [Sequence]
evalCoreMany env cs = [ rationalPrefix $ evalCore' env c | c <- cs ]

runPrgs :: KnownNat n => [Env n] -> [Core] -> [Sequence]
runPrgs envs progs =
    concat ( [ evalCoreMany env progs
             | env <- envs
             ] `using` parBuffer 256 rdeepseq )

hops :: KnownNat n => Proxy n -> Input n -> IO Output
hops n inp =
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
          return $ Entries (zipWith3 Entry ps results (trails ++ repeat []))
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
          readInput t c >>= hops (Proxy :: Proxy n) >>= printOutput
