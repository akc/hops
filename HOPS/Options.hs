-- |
-- Copyright   : Anders Claesson 2015, 2016
-- Maintainer  : Anders Claesson <anders.claesson@gmail.com>
-- License     : BSD-3
--
-- Command line options for hops.

module HOPS.Options
    ( Options (..)
    , getOptions
    ) where

import Data.Monoid
import Options.Applicative

-- | Command line options:
data Options = Options
    {
      script  :: String
    , prec    :: Int
    , minPrec :: Int
    , int     :: Bool
    , tagSeqs :: Maybe Int
    , forAll  :: Bool
    , update  :: Bool
    , version :: Bool
    , program :: [String]
    }

-- | Parse command line options.
optionsParser :: Parser Options
optionsParser =
  abortOption ShowHelpText (long "help") <*> (Options
    <$> strOption
        ( short 'f'
       <> long "script"
       <> metavar "FILENAME"
       <> value ""
       <> help "Filename of script to run" )
    <*> option auto
        ( long "prec"
       <> metavar "N"
       <> value 15
       <> help "Generating function precision [default: 15]" )
    <*> option auto
        ( long "min-prec"
       <> metavar "N"
       <> value 0
       <> help "Smallest precision of returned sequences [default: 0]" )
    <*> switch
        ( long "int"
       <> help "Only return sequences whose entries are integers" )
    <*> optional (option auto
        ( long "tag"
       <> metavar "N"
       <> help "Read sequences from stdin and tag them, starting at N" ))
    <*> switch
        ( long "forall"
       <> help "Run program(s) on all sequences in the local DB" )
    <*> switch
        ( long "update"
       <> help "Update the local database" )
    <*> switch
        ( long "version"
       <> help "Show version info" )
    <*> many (argument str (metavar "PROGRAMS...")))

-- | Run the command line options parser (above).
getOptions :: IO Options
getOptions = execParser (info optionsParser fullDesc)
