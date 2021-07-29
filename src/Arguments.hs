{-# LANGUAGE ScopedTypeVariables #-}
module Arguments
  ( ProgramOptions(..)
  , programOptions
  ) where

import Options.Applicative
import Text.Read (readMaybe)

data ProgramOptions = ProgramOptions
  { outputDir :: FilePath
  , rootNodePath :: Maybe FilePath
  , folderList :: [FilePath]
  }

optionParser :: Parser ProgramOptions
optionParser = ProgramOptions
  <$> strArgument (
             help "output folder (will be created if needed)"
          <> metavar "OUTPUT-FOLDER" )
  <*> option (maybeReader $ Just . Just) (
             long "root-node"
          <> value Nothing
          <> showDefault
          <> help "Root node in output graph" )
  <*> some (strArgument (
             help "Folders containing Org files created with Org Roam"
          <> metavar "ROAM-FOLDERS" ))

parseToMaybe :: (Read a) => String -> Maybe (Maybe a)
parseToMaybe = fmap Just . readMaybe

programOptions :: ParserInfo ProgramOptions
programOptions = info (optionParser <**> helper) (
    progDesc "Export an Org-Roam wiki to HTML")
