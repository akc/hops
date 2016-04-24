{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Data.Monoid
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson
import Control.Applicative
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
import HOPS.GF.Series

versionString :: String
versionString = "0.6.0"

seqsURL :: String
seqsURL = "https://oeis.org/stripped.gz"

data Input (n :: Nat)
    = RunPrgs (Env n) [Prg] [CorePrg] [Entry]
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

readEntries :: IO [Entry]
readEntries = do
  let decodeErr = fromMaybe (error "error decoding JSON") . decode
  map (decodeErr . BL.fromStrict) <$> readStdin

readPrgs :: Options -> IO ([Prg], [CorePrg])
readPrgs opts = do
    prgs <- filter (not . null . commands) . map parsePrgErr <$>
              if script opts == ""
                  then return (map B.pack (program opts))
                  else lines' <$> BL.readFile (script opts)
    return (prgs, map core prgs)

readDB :: Config -> IO [Entry]
readDB =
    let mkEntry (ANum a, s) = Entry (aNumPrg a) s []
    in fmap (map mkEntry . parseStripped . unDB) . readSeqDB

readInput :: KnownNat n => Options -> Config -> IO (Input n)
readInput opts cfg
    | version opts = return Empty
    | update opts = return $ UpdateDBs (hopsDir cfg) (seqDBPath cfg)
    | isJust (tagSeqs opts) =
        TagSeqs (fromJust (tagSeqs opts)) . map parseIntegerSeqErr <$> readStdin
    | otherwise = do
        (prgs, cprgs) <- readPrgs opts
        inp <- if forAll opts
               then readDB cfg
               else if "stdin" `elem` (vars =<< cprgs)
                    then readEntries
                    else return []
        db  <- if null (anums =<< cprgs) then return emptyANumDB else readANumDB cfg
        return $ RunPrgs (Env db M.empty) prgs cprgs inp

printOutput :: Output -> IO ()
printOutput NOP = return ()
printOutput (Entries es) = mapM_ (BL.putStrLn . encode) es

stdEnv :: KnownNat n => Proxy n -> Env n -> Sequence -> Env n
stdEnv n (Env a v) s = Env a $ M.insert "stdin" (series n (map Val s)) v

parbuf256 :: NFData a => Strategy [a]
parbuf256 = parBuffer 256 rdeepseq

runPrgs :: KnownNat n => [Env n] -> [CorePrg] -> [Sequence]
runPrgs es ps =
    concat ([rationalPrefix <$> evalCorePrgs e ps | e <- es] `using` parbuf256)

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
          return $ Entries [ Entry (tagPrg i) t [] | (i, t) <- zip [i0 .. ] ts ]

      Empty -> putStrLn ("hops " ++ versionString) >> return NOP

      RunPrgs env prgs cprgs entries ->
          return $ Entries (zipWith3 Entry ps results (trails ++ repeat []))
        where
          trails = map getTrail entries
          results = runPrgs envs cprgs
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
    let d = fromIntegral (prec t)
    case someNatVal d of
      Nothing -> error $ show d ++ " not a valid prec"
      Just (SomeNat (_ :: Proxy n)) ->
          readInput t c >>= hops (Proxy :: Proxy n) >>= printOutput
