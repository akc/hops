{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
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
versionString = "0.5.0"

seqsURL :: String
seqsURL = "https://oeis.org/stripped.gz"

type Prec = Int

data Input (n :: Nat)
    = RunPrgs (Env n) [Prg] [CorePrg] [Entry]
    | TagSeqs Int [Sequence]
    | DumpSeqDB Prec [Entry]
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

readInput :: KnownNat n => Options -> Config -> IO (Input n)
readInput opts cfg
    | version opts = return Empty

    | dumpSeqs opts =
          DumpSeqDB (prec opts)
        . map (\(ANum a, s) -> Entry (aNumPrg a) s [])
        . parseStripped
        . unDB <$> readSeqDB cfg

    | update opts =
        return $ UpdateDBs (hopsDir cfg) (seqDBPath cfg)

    | isJust (tagSeqs opts) =
        TagSeqs (fromJust (tagSeqs opts)) . map parseIntegerSeqErr <$> readStdin

    | otherwise = do
        (prgs, cprgs) <- readPrgs opts
        inp <- if "stdin" `elem` (vars =<< cprgs) then readEntries else return []
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

      DumpSeqDB precn es ->
          return $ Entries
              [ Entry p t trail
              | Entry p s trail <- es
              , let t = take precn s
              , not (null t)
              ]

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
    case prec t of
      0              -> readInput t c >>= hops (Proxy :: Proxy     0) >>= printOutput
      1              -> readInput t c >>= hops (Proxy :: Proxy     1) >>= printOutput
      2              -> readInput t c >>= hops (Proxy :: Proxy     2) >>= printOutput
      3              -> readInput t c >>= hops (Proxy :: Proxy     3) >>= printOutput
      4              -> readInput t c >>= hops (Proxy :: Proxy     4) >>= printOutput
      5              -> readInput t c >>= hops (Proxy :: Proxy     5) >>= printOutput
      6              -> readInput t c >>= hops (Proxy :: Proxy     6) >>= printOutput
      7              -> readInput t c >>= hops (Proxy :: Proxy     7) >>= printOutput
      8              -> readInput t c >>= hops (Proxy :: Proxy     8) >>= printOutput
      9              -> readInput t c >>= hops (Proxy :: Proxy     9) >>= printOutput
      10             -> readInput t c >>= hops (Proxy :: Proxy    10) >>= printOutput
      11             -> readInput t c >>= hops (Proxy :: Proxy    11) >>= printOutput
      12             -> readInput t c >>= hops (Proxy :: Proxy    12) >>= printOutput
      13             -> readInput t c >>= hops (Proxy :: Proxy    13) >>= printOutput
      14             -> readInput t c >>= hops (Proxy :: Proxy    14) >>= printOutput
      15             -> readInput t c >>= hops (Proxy :: Proxy    15) >>= printOutput
      16             -> readInput t c >>= hops (Proxy :: Proxy    16) >>= printOutput
      17             -> readInput t c >>= hops (Proxy :: Proxy    17) >>= printOutput
      18             -> readInput t c >>= hops (Proxy :: Proxy    18) >>= printOutput
      19             -> readInput t c >>= hops (Proxy :: Proxy    19) >>= printOutput
      20             -> readInput t c >>= hops (Proxy :: Proxy    20) >>= printOutput
      21             -> readInput t c >>= hops (Proxy :: Proxy    21) >>= printOutput
      22             -> readInput t c >>= hops (Proxy :: Proxy    22) >>= printOutput
      23             -> readInput t c >>= hops (Proxy :: Proxy    23) >>= printOutput
      24             -> readInput t c >>= hops (Proxy :: Proxy    24) >>= printOutput
      25             -> readInput t c >>= hops (Proxy :: Proxy    25) >>= printOutput
      26             -> readInput t c >>= hops (Proxy :: Proxy    26) >>= printOutput
      27             -> readInput t c >>= hops (Proxy :: Proxy    27) >>= printOutput
      28             -> readInput t c >>= hops (Proxy :: Proxy    28) >>= printOutput
      29             -> readInput t c >>= hops (Proxy :: Proxy    29) >>= printOutput
      30             -> readInput t c >>= hops (Proxy :: Proxy    30) >>= printOutput
      31             -> readInput t c >>= hops (Proxy :: Proxy    31) >>= printOutput
      32             -> readInput t c >>= hops (Proxy :: Proxy    32) >>= printOutput
      33             -> readInput t c >>= hops (Proxy :: Proxy    33) >>= printOutput
      34             -> readInput t c >>= hops (Proxy :: Proxy    34) >>= printOutput
      35             -> readInput t c >>= hops (Proxy :: Proxy    35) >>= printOutput
      36             -> readInput t c >>= hops (Proxy :: Proxy    36) >>= printOutput
      37             -> readInput t c >>= hops (Proxy :: Proxy    37) >>= printOutput
      38             -> readInput t c >>= hops (Proxy :: Proxy    38) >>= printOutput
      39             -> readInput t c >>= hops (Proxy :: Proxy    39) >>= printOutput
      40             -> readInput t c >>= hops (Proxy :: Proxy    40) >>= printOutput
      41             -> readInput t c >>= hops (Proxy :: Proxy    41) >>= printOutput
      42             -> readInput t c >>= hops (Proxy :: Proxy    42) >>= printOutput
      43             -> readInput t c >>= hops (Proxy :: Proxy    43) >>= printOutput
      44             -> readInput t c >>= hops (Proxy :: Proxy    44) >>= printOutput
      45             -> readInput t c >>= hops (Proxy :: Proxy    45) >>= printOutput
      46             -> readInput t c >>= hops (Proxy :: Proxy    46) >>= printOutput
      47             -> readInput t c >>= hops (Proxy :: Proxy    47) >>= printOutput
      48             -> readInput t c >>= hops (Proxy :: Proxy    48) >>= printOutput
      49             -> readInput t c >>= hops (Proxy :: Proxy    49) >>= printOutput
      50             -> readInput t c >>= hops (Proxy :: Proxy    50) >>= printOutput
      51             -> readInput t c >>= hops (Proxy :: Proxy    51) >>= printOutput
      52             -> readInput t c >>= hops (Proxy :: Proxy    52) >>= printOutput
      53             -> readInput t c >>= hops (Proxy :: Proxy    53) >>= printOutput
      54             -> readInput t c >>= hops (Proxy :: Proxy    54) >>= printOutput
      55             -> readInput t c >>= hops (Proxy :: Proxy    55) >>= printOutput
      56             -> readInput t c >>= hops (Proxy :: Proxy    56) >>= printOutput
      57             -> readInput t c >>= hops (Proxy :: Proxy    57) >>= printOutput
      58             -> readInput t c >>= hops (Proxy :: Proxy    58) >>= printOutput
      59             -> readInput t c >>= hops (Proxy :: Proxy    59) >>= printOutput
      60             -> readInput t c >>= hops (Proxy :: Proxy    60) >>= printOutput
      61             -> readInput t c >>= hops (Proxy :: Proxy    61) >>= printOutput
      62             -> readInput t c >>= hops (Proxy :: Proxy    62) >>= printOutput
      63             -> readInput t c >>= hops (Proxy :: Proxy    63) >>= printOutput
      64             -> readInput t c >>= hops (Proxy :: Proxy    64) >>= printOutput
      65             -> readInput t c >>= hops (Proxy :: Proxy    65) >>= printOutput
      66             -> readInput t c >>= hops (Proxy :: Proxy    66) >>= printOutput
      67             -> readInput t c >>= hops (Proxy :: Proxy    67) >>= printOutput
      68             -> readInput t c >>= hops (Proxy :: Proxy    68) >>= printOutput
      69             -> readInput t c >>= hops (Proxy :: Proxy    69) >>= printOutput
      70             -> readInput t c >>= hops (Proxy :: Proxy    70) >>= printOutput
      71             -> readInput t c >>= hops (Proxy :: Proxy    71) >>= printOutput
      72             -> readInput t c >>= hops (Proxy :: Proxy    72) >>= printOutput
      73             -> readInput t c >>= hops (Proxy :: Proxy    73) >>= printOutput
      74             -> readInput t c >>= hops (Proxy :: Proxy    74) >>= printOutput
      75             -> readInput t c >>= hops (Proxy :: Proxy    75) >>= printOutput
      76             -> readInput t c >>= hops (Proxy :: Proxy    76) >>= printOutput
      77             -> readInput t c >>= hops (Proxy :: Proxy    77) >>= printOutput
      78             -> readInput t c >>= hops (Proxy :: Proxy    78) >>= printOutput
      79             -> readInput t c >>= hops (Proxy :: Proxy    79) >>= printOutput
      80             -> readInput t c >>= hops (Proxy :: Proxy    80) >>= printOutput
      81             -> readInput t c >>= hops (Proxy :: Proxy    81) >>= printOutput
      82             -> readInput t c >>= hops (Proxy :: Proxy    82) >>= printOutput
      83             -> readInput t c >>= hops (Proxy :: Proxy    83) >>= printOutput
      84             -> readInput t c >>= hops (Proxy :: Proxy    84) >>= printOutput
      85             -> readInput t c >>= hops (Proxy :: Proxy    85) >>= printOutput
      86             -> readInput t c >>= hops (Proxy :: Proxy    86) >>= printOutput
      87             -> readInput t c >>= hops (Proxy :: Proxy    87) >>= printOutput
      88             -> readInput t c >>= hops (Proxy :: Proxy    88) >>= printOutput
      89             -> readInput t c >>= hops (Proxy :: Proxy    89) >>= printOutput
      90             -> readInput t c >>= hops (Proxy :: Proxy    90) >>= printOutput
      91             -> readInput t c >>= hops (Proxy :: Proxy    91) >>= printOutput
      92             -> readInput t c >>= hops (Proxy :: Proxy    92) >>= printOutput
      93             -> readInput t c >>= hops (Proxy :: Proxy    93) >>= printOutput
      94             -> readInput t c >>= hops (Proxy :: Proxy    94) >>= printOutput
      95             -> readInput t c >>= hops (Proxy :: Proxy    95) >>= printOutput
      96             -> readInput t c >>= hops (Proxy :: Proxy    96) >>= printOutput
      97             -> readInput t c >>= hops (Proxy :: Proxy    97) >>= printOutput
      98             -> readInput t c >>= hops (Proxy :: Proxy    98) >>= printOutput
      99             -> readInput t c >>= hops (Proxy :: Proxy    99) >>= printOutput
      p | p <=   128 -> readInput t c >>= hops (Proxy :: Proxy   128) >>= printOutput
        | p <=   256 -> readInput t c >>= hops (Proxy :: Proxy   256) >>= printOutput
        | p <=   512 -> readInput t c >>= hops (Proxy :: Proxy   512) >>= printOutput
        | p <=  1024 -> readInput t c >>= hops (Proxy :: Proxy  1024) >>= printOutput
        | p <=  2048 -> readInput t c >>= hops (Proxy :: Proxy  2048) >>= printOutput
        | p <=  4096 -> readInput t c >>= hops (Proxy :: Proxy  4096) >>= printOutput
        | p <=  8192 -> readInput t c >>= hops (Proxy :: Proxy  8192) >>= printOutput
        | p <= 16384 -> readInput t c >>= hops (Proxy :: Proxy 16384) >>= printOutput
        | p <= 32768 -> readInput t c >>= hops (Proxy :: Proxy 32768) >>= printOutput
        | p <= 65536 -> readInput t c >>= hops (Proxy :: Proxy 65536) >>= printOutput
        | otherwise  -> error "max-precision is 65535"
