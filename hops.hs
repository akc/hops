{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--

module Main (main) where

import GHC.TypeLits
import Data.Proxy
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad
import Control.Parallel.Strategies
import System.Directory
import System.IO
import HOPS.Entry
import HOPS.OEIS
import HOPS.Options
import HOPS.Config
import HOPS.Download
import HOPS.Utils
import HOPS.DB
import HOPS.GF
import HOPS.GF.Series
import HOPS.GF.Transform

nameVer = "hops 0.1.0" :: String
strpdURL = "https://oeis.org/stripped.gz" :: URL

type Prec = Int
type TrName = B.ByteString

data Input (n :: Nat)
    = RunPrgs Prec (Env n) [Prg Integer] [Entry]
    | TagSeqs Int [PackedSeq]
    | DumpSeqDB Prec [PackedEntry]
    | UpdateDBs FilePath FilePath
    | ToJSON [PackedEntry]
    | FromJSON [PackedEntry]
    | ListTransforms [TrName]
    | Empty

data Output
    = Entries [PackedEntry]
    | JSONEntries [PackedEntry]
    | Transforms [TrName]
    | NOP

lines' :: BL.ByteString -> [B.ByteString]
lines' = filter (not . B.null) . map BL.toStrict . BL.lines

readStdin :: IO [B.ByteString]
readStdin = lines' <$> BL.getContents

readPrgs :: Options -> IO [Prg Integer]
readPrgs opts =
    filter (not . null . commands) . map parsePrgErr <$>
        if script opts == ""
            then return (map B.pack (terms opts))
            else lines' <$> BL.readFile (script opts)

readInput :: KnownNat n => Options -> Config -> IO (Input n)
readInput opts cfg
    | version opts = return Empty

    | dumpSeqs opts =
          DumpSeqDB (prec opts)
        . map (\(ANum a, s) -> PackedEntry (PPrg a) s)
        . parseStripped
        . unDB <$> readSeqDB cfg

    | update opts =
        return $ UpdateDBs (hopsDir cfg) (seqDBPath cfg)

    | listTransforms opts = return $ ListTransforms transforms

    | tojson opts = ToJSON . map parsePackedEntryErr <$> readStdin

    | fromjson opts = do
        let decodeErr = fromMaybe (error "error decoding JSON") . decode
        FromJSON . map (decodeErr . BL.fromStrict) <$> readStdin

    | isJust (tagSeqs opts) =
        TagSeqs (fromJust (tagSeqs opts)) . map PSeq <$> readStdin

    | otherwise = do
        prgs <- readPrgs opts
        inp  <- if "stdin" `elem` (vars =<< prgs) then readStdin else return []
        db   <- if null (anums =<< prgs) then return emptyANumDB else readANumDB cfg
        return $ RunPrgs (prec opts) (Env db M.empty) prgs (map parseEntry inp)

printOutput :: Output -> IO ()
printOutput NOP = return ()
printOutput (Transforms ts) = mapM_ B.putStrLn ts
printOutput (JSONEntries es) = mapM_ (BL.putStrLn . encode) es
printOutput (Entries es) =
    forM_ es $ \(PackedEntry (PPrg p) (PSeq s)) ->
        B.putStrLn $ p <> " => {" <> s <> "}"

stdEnv :: KnownNat n => Proxy n -> Env n -> [Rational] -> Env n
stdEnv n (Env a v) s = Env a $ M.insert "stdin" (series n (map Val s)) v

parbuf256 :: NFData a => Strategy [a]
parbuf256 = parBuffer 256 rdeepseq

runPrgs :: KnownNat n => Int -> [Env n] -> [Prg Integer] -> [PackedSeq]
runPrgs k es ps =
    let getCoeffs = packSeq . take k . rationalPrefix . snd
    in concat ([ getCoeffs <$> evalPrgs e ps | e <- es] `using` parbuf256)

hops :: KnownNat n => Proxy n -> Input n -> IO Output
hops n inp =
    case inp of

      DumpSeqDB precn es ->
          return $ Entries
              [ PackedEntry p (packSeq t)
              | PackedEntry p (PSeq s) <- es
              , let t = take precn (parseSeqErr s)
              , not (null t)
              ]

      UpdateDBs hopsdir sdbPath -> do
          createDirectoryIfMissing False hopsdir
          let msg1 = "Downloading " ++ strpdURL ++ ": "
          putStr msg1 >> hFlush stdout
          download (length msg1) strpdURL sdbPath >> putStrLn ""
          return NOP

      TagSeqs i0 ts ->
          return $ Entries
              [ PackedEntry (PPrg ("TAG" <> pad 6 i)) t
              | (i, t) <- zip [i0 .. ] ts
              ]

      ListTransforms ts -> return $ Transforms ts

      ToJSON es -> return $ JSONEntries es

      FromJSON es -> return $ Entries es

      Empty -> putStrLn nameVer >> return NOP

      RunPrgs precn env prgs entries ->
          return $ Entries (zipWith PackedEntry (packPrg <$> ps) results)
        where
          results = runPrgs precn envs prgs
          (ps, envs) = case entries of
            [] -> ( prgs, [env] )
            _  -> ( concat [[ q <> p | p <- prgs ] | e <- entries, let q = getPrg e]
                  , map (stdEnv n env . getSeq) entries
                  )

-- | Main function and entry point for hops.
main :: IO ()
main = do
    c <- getConfig
    t <- getOptions
    case prec t of
      p | p <    4  -> readInput t c >>= hops (Proxy :: Proxy    4) >>= printOutput
        | p <    8  -> readInput t c >>= hops (Proxy :: Proxy    8) >>= printOutput
        | p <   12  -> readInput t c >>= hops (Proxy :: Proxy   12) >>= printOutput
        | p <   16  -> readInput t c >>= hops (Proxy :: Proxy   16) >>= printOutput
        | p <   20  -> readInput t c >>= hops (Proxy :: Proxy   20) >>= printOutput
        | p <   24  -> readInput t c >>= hops (Proxy :: Proxy   24) >>= printOutput
        | p <   28  -> readInput t c >>= hops (Proxy :: Proxy   28) >>= printOutput
        | p <   32  -> readInput t c >>= hops (Proxy :: Proxy   32) >>= printOutput
        | p <   64  -> readInput t c >>= hops (Proxy :: Proxy   64) >>= printOutput
        | p <   96  -> readInput t c >>= hops (Proxy :: Proxy   96) >>= printOutput
        | p <  128  -> readInput t c >>= hops (Proxy :: Proxy  128) >>= printOutput
        | p <  256  -> readInput t c >>= hops (Proxy :: Proxy  256) >>= printOutput
        | p <  512  -> readInput t c >>= hops (Proxy :: Proxy  512) >>= printOutput
        | p < 1024  -> readInput t c >>= hops (Proxy :: Proxy 1024) >>= printOutput
        | p < 2048  -> readInput t c >>= hops (Proxy :: Proxy 2048) >>= printOutput
        | otherwise -> error "max-precision is 2047"
