{-|
Module      : IndexerOptions
Description : Command line arguments parsing for the indexer.

Command line arguments parsing for the indexer.
-}
module IndexerOptions
  ( SearchOptions(..)
  , IndexOptions(..)
  , Command(..)
  , Options(..)
  , parseOptions
  ) where

import           Data.Semigroup      ((<>))
import           Options.Applicative

indexFileOption :: Parser FilePath
indexFileOption =
  strOption
    (long "index-file" <> short 'f' <> metavar "INDEX-FILE" <> value "index.inx" <>
     help "Use FILE as index" <>
     showDefault)

execOption :: Parser String
execOption =
  strOption
    (long "exec" <> short 'e' <> metavar "COMMAND" <> value "" <>
     help
       "Execute command on each found file. In the command, {} is substitued for the file path")

searchPhraseArgument :: Parser String
searchPhraseArgument = argument str (metavar "PHRASE")

-- |Search options
data SearchOptions = SearchOptions
  { argPhrase :: String
  , optExec   :: String
  } deriving (Show)

searchOptions :: Parser SearchOptions
searchOptions =
  helper <*> (SearchOptions <$> searchPhraseArgument <*> execOption)

resourceArguments :: Parser [String]
resourceArguments =
  some $
  argument
    str
    (metavar "RESOURCES..." <> help "Paths or URLs to Markdown or HTML files")

appendToIndexSwitch :: Parser Bool
appendToIndexSwitch =
  switch $
  long "append-index" <> short 'a' <>
  help "Append to index file instead of replacing it" <>
  showDefault

-- |Index options
data IndexOptions = IndexOptions
  { argResources :: [String]
  , switchAppend :: Bool
  } deriving (Show)

indexOptions :: Parser IndexOptions
indexOptions =
  helper <*> (IndexOptions <$> resourceArguments <*> appendToIndexSwitch)

-- |The main command for the executable.
data Command
  = Search SearchOptions
  | Index IndexOptions
  deriving (Show)

-- |All the options for the executable.
data Options = Options
  { optCommand   :: Command
  , optIndexFile :: FilePath
  } deriving (Show)

indexCommand :: Mod CommandFields Options
indexCommand =
  command
    "index"
    (info
       (Options <$> (Index <$> indexOptions) <*> indexFileOption)
       (progDesc "Index Markdown or HTML files"))

searchCommand :: Mod CommandFields Options
searchCommand =
  command
    "search"
    (info
       (Options <$> (Search <$> searchOptions) <*> indexFileOption)
       (progDesc "Search indexed files"))

options :: Parser Options
options = hsubparser (indexCommand <> searchCommand)

-- |Parses command line arguments. Provides additional information when the arguments are invalid.
parseOptions :: IO Options
parseOptions =
  execParser
    (info
       (helper <*> options)
       (progDesc "Index and search Markdown and HTML headings" <>
        header "md-html-indexer"))
