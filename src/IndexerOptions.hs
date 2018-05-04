module IndexerOptions
  ( SearchOptions(..)
  , IndexOptions(..)
  , Command(..)
  , Options(..)
  , parseOptions
  ) where

import           Control.Monad
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

newtype IndexOptions = IndexOptions
  { argResources :: [String]
  } deriving (Show)

indexOptions :: Parser IndexOptions
indexOptions = helper <*> (IndexOptions <$> resourceArguments)

data Command
  = Search SearchOptions
  | Index IndexOptions
  deriving (Show)

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

parseOptions :: IO Options
parseOptions =
  execParser
    (info
       (helper <*> options)
       (progDesc "Index and search Markdown and HTML headings" <>
        header "md-html-indexer"))
