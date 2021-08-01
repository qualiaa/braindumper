{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Monad(forM_, unless, when)
import Control.Monad.Except (runExceptT)
import Control.Monad.Extra (whenJust)
import Data.Default (def)
import Data.Either (isLeft, rights)
import Data.Either.Extra (fromRight')
import Data.List (find)
import Data.List.Extra (mconcatMap)
import Data.Text (Text)
import Options.Applicative (execParser)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (equalFilePath, (</>))
import System.IO  (hPutStrLn, stderr)
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Definition (Inline(Span), Pandoc)
import Text.Pandoc.Walk (query)
import Text.Pandoc.Writers (writeHtml5String)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Arguments (programOptions, ProgramOptions(..))
import Graph (findBacklinks, findConnectedNodes)
import Input (findOrgFiles, getFileData)
import Processing (buildIndex, processFileContents)
import Output ( idToOutputName, fileToOutputPath, safeCreateLink)
import Types (FileId(..), FileData(..))
import Util (whenNothing)

writerOptions = def

main :: IO ()
main = runProgram =<< execParser programOptions

runProgram :: ProgramOptions -> IO ()
runProgram ProgramOptions{..} = do
  fileList <- concat <$> mapM findOrgFiles folderList

  fileParseResults <- mapM (runExceptT . getFileData) fileList

  let fileData = rights fileParseResults
  rootNode <- case rootNodePath of
    Just r -> whenNothing (find (equalFilePath r . filePath) fileData) $
                          putStrLn ("Root node not found: " ++ r)
    Nothing -> return Nothing

  let flatGraph' = M.fromList $ zip (map fileId fileData) fileData
      flatGraph = maybe
        flatGraph'
        (M.restrictKeys flatGraph' . findConnectedNodes flatGraph')
        (fileId <$> rootNode)

  -- Report errors in file parsing
  forM_ (zip fileList fileParseResults) $ \(path, r) ->
    when (isLeft r) $ let (Left err) = r in hPutStrLn stderr (path ++ ": " ++ err)

  createDirectoryIfMissing False outputDir

  forM_ (M.elems flatGraph)  $ \currentFile -> do
    html <- convertFileToHtml flatGraph currentFile
    let outputPath = fileToOutputPath outputDir currentFile
    TIO.writeFile outputPath html

  -- Create symlink to root node
  whenJust rootNode $ \r ->
    let target = idToOutputName $ fileId r
        link = outputDir </> "root.html"
    in safeCreateLink target link

  buildIndex (M.elems flatGraph) $ outputDir </> "index.html"

findSpuriousLinks :: Pandoc -> [Text]
findSpuriousLinks = query (\case
    (Span (_, elem "spurious-link" -> True,
              lookup "target" -> Just target)
     text) -> [target]
    _ -> [])

convertFileToHtml :: M.Map FileId FileData -> FileData -> IO Text
convertFileToHtml flatGraph FileData{..} = do
  -- TODO compute backlinks once for all files
  -- TODO compute failedIds once from backlinks
  let backlinks = findBacklinks flatGraph fileId

      (processedAST, failedIds) =
        processFileContents flatGraph backlinks fileContents

      spuriousLinks = findSpuriousLinks processedAST

      badLinksString = mconcatMap ("\n" <>) $
        map getIdText failedIds ++ spuriousLinks

  -- TODO remove IO from this function
  -- Report missing IDs
  unless (null failedIds && null spuriousLinks) $
      let err = mconcat [T.pack filePath, " contains invalid links: ",
                         badLinksString]
      in  TIO.hPutStrLn stderr err

  return . fromRight' . runPure $ writeHtml5String writerOptions processedAST
