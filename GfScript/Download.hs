{-# LANGUAGE CPP #-}
-- |
-- Copyright   : Anders Claesson 2015
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
module GfScript.Download (download, requestPage) where

import Numeric
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Conduit hiding (($$))
import Data.Conduit.Zlib (ungzip)
import qualified Data.Conduit.Binary as CB
import Network.HTTP.Conduit hiding (Proxy)
import Network.HTTP.Types (hContentLength)
import Control.Monad.IO.Class (liftIO)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import GfScript.OEIS (URL)
import System.Console.ANSI
import System.IO

-- | Request a page at a given URL with specified key-value pairs (query
-- string).
requestPage :: URL -> [(ByteString, ByteString)] -> IO ByteString
requestPage url kvs = do
    req <- setQueryString [(k, Just v) | (k,v) <- kvs] <$> parseUrl url
    res <- withManager (httpLbs req)
    return $ BL.toStrict (responseBody res)

-- | Download a file at a given URL showing a progress indicator at the
-- given column, and save it at a specified path.
download :: Int -> URL -> FilePath -> IO ()
download col url fpath = withManager $ \manager -> do
    req <- parseUrl url
    res <- http req manager
    let Just cl = lookup hContentLength (responseHeaders res)
    let n = read (B.unpack cl) :: Int
    responseBody res $$+- progress n 0 =$ ungzip =$ CB.sinkFile fpath
  where
    progress total acc = await >>= maybe (return ()) (\chunk -> do
        let acc' = acc + B.length chunk
        let percent = fromIntegral (100*acc') / fromIntegral total :: Double
        liftIO $ setCursorColumn col
        liftIO $ putStr $ showFFloat (Just 2) percent "" ++ "%"
        liftIO $ hFlush stdout
        yield chunk
        progress total acc')
