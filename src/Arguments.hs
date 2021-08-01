{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Arguments
  ( ProgramOptions(..)
  , programOptions
  ) where

import Options.Applicative

data ProgramOptions = ProgramOptions
  { outputDir :: FilePath
  , rootNodePath :: Maybe FilePath
  , pandocTemplatePath :: Maybe FilePath
  , folderList :: [FilePath]
  }

optionParser :: Parser ProgramOptions
optionParser = ProgramOptions
  <$> strArgument (
             help "output folder (will be created if needed)"
          <> metavar "OUTPUT-FOLDER" )
  <*> option toMaybe (
             long "root-node"
          <> value Nothing
          <> showDefault
          <> help "Root node in output graph" )
  <*> option toMaybe (
             long "template"
          <> value Nothing
          <> showDefault
          <> help "Path of HTML pandoc template for pages")
  <*> some (strArgument (
             help "Folders containing Org files created with Org Roam"
          <> metavar "ROAM-FOLDERS" ))

programOptions :: ParserInfo ProgramOptions
programOptions = info (optionParser <**> helper) (
    progDesc "Export an Org-Roam wiki to HTML")

toMaybe = maybeReader $ Just . Just
