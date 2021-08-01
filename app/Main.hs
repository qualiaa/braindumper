{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad(forM_, unless, when)
import Control.Monad.Except (runExceptT, throwError)
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
import Text.DocTemplates (toVal, Context(..), Val(..))
import Text.Pandoc (Inline(Span), Pandoc, PandocError(..), WriterOptions(..), runPure, runIO)
import Text.Pandoc.Templates (compileTemplate,
                              getTemplate,
                              compileDefaultTemplate, runWithPartials)
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
    html <- convertFileToHtml flatGraph pandocTemplatePath currentFile
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

convertFileToHtml :: M.Map FileId FileData -> Maybe FilePath -> FileData -> IO Text
convertFileToHtml flatGraph template FileData{..} = do
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

  -- Reproduced from https://github.com/jgm/pandoc/blob/60974538b25657c9aa37e72cc66ca3957912ddec/src/Text/Pandoc/App/OutputSettings.hs#L168
  (Right template) <- runIO $ case template of
    Nothing -> Just <$> compileDefaultTemplate "html"
    Just path -> do
      res <- getTemplate path >>= runWithPartials . compileTemplate path
      case res of
        Left e -> throwError $ PandocTemplateError $ T.pack e
        Right t -> return $ Just t
  let writerOptions = def { writerTemplate = template
                          , writerVariables = setListVariable "css"
                                (map T.pack ["style.css"]) mempty
                          }

  return . fromRight' . runPure $ writeHtml5String writerOptions processedAST


-- Reproduced from https://github.com/jgm/pandoc/blob/60974538b25657c9aa37e72cc66ca3957912ddec/src/Text/Pandoc/App/OutputSettings.hs#L115
setListVariable _ [] ctx = ctx
setListVariable k vs ctx =
  let ctxMap = unContext ctx in
    Context $
    case M.lookup k ctxMap of
      Just (ListVal xs) -> M.insert k
                           (ListVal $ xs ++ map toVal vs) ctxMap
      Just v -> M.insert k (ListVal $ v : map toVal vs) ctxMap
      Nothing -> M.insert k (toVal vs) ctxMap
