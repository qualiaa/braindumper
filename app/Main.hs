{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Monad (forM_, when)
import Control.Monad.Except
import Control.Monad.Writer
-- import Control.Monad.Trans.Maybe
-- import Control.Monad.Trans.Class (lift)
import Data.Default (def)
import Data.Either (isLeft, rights)
import Data.List (find, intercalate, foldl')
import Data.Maybe (isJust, isNothing, mapMaybe, maybe)
import Data.Text (Text)
import Options.Applicative (execParser)
import System.Directory ( createDirectoryIfMissing
                        , createFileLink
                        , doesPathExist
                        , getCurrentDirectory
                        , listDirectory
                        , removeFile
                        )
import System.Environment (getArgs)
import System.FilePath (isExtensionOf, equalFilePath, (</>), (<.>))
import System.IO  (hPutStrLn, stderr)
import Text.Pandoc.Class (runIO, runPure)
import Text.Pandoc.Definition ( MetaValue(MetaInlines)
                              , Block(Div, Header, Para)
                              , Inline(..)
                              , Meta(..)
                              , Pandoc(..)
                              )
import Text.Pandoc.Builder (header)
import Text.Pandoc.Walk (query, walk, walkM)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Readers (readOrg)
import Text.Pandoc.Writers (writePlain, writeHtml5String)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as LazyTIO
import Arguments

newtype FileId = FileId { getIdText :: Text } deriving (Eq, Ord, Show)
type ExceptIO = ExceptT String IO
data FileData = FileData
  { fileId :: FileId
  , filePath :: FilePath
  , fileTitle :: Text
  , fileTitlePandoc :: [Inline]
  , fileContents :: Pandoc
  , fileLinkedIds :: S.Set FileId
  } deriving (Eq, Show)

-- TODO Use Pandoc.Builder helpers

writerOptions = def
readerOptions = def --{readerStandalone=True}


main :: IO ()
main = runProgram =<< execParser programOptions

runProgram :: ProgramOptions -> IO ()
runProgram ProgramOptions{..} = do
  fileList <- concat <$> mapM findOrgFiles folderList

  fileParseResults <- mapM (runExceptT . getFileData) fileList
  -- TODO: Warn if root provided but not found
  let fileData = rights fileParseResults
      rootNode = do
        r <- rootNodePath
        find (\file -> filePath file `equalFilePath` r) fileData
      idToData' = M.fromList $ zip (map fileId fileData) fileData
      idToData = maybe
          idToData'
          (M.restrictKeys idToData' . extractSubgraph idToData')
          (fileId <$> rootNode)

  -- Report errors in file parsing
  forM_ (zip fileList fileParseResults) $ \(path, r) ->
    when (isLeft r) $ let (Left err) = r in hPutStrLn stderr (path ++ ": " ++ err)

  createDirectoryIfMissing False outputDir

  forM_ (M.elems idToData)  $ \currentFile@FileData{..} -> do
    -- TODO: Neaten this up
    let (fileContents', failedIds) = runWriter $
            walkM (fixLink idToData) fileContents
        fileContents'' = walk demoteHeaders fileContents'
        failedIdString = intercalate ", " $ map (show . getIdText) failedIds
        outputPath = fileToOutputPath outputDir currentFile

        fileContents''' = addBacklinks fileContents'' $
           findBacklinks idToData fileId
        fileContents'''' = addTitle fileTitlePandoc fileContents'''

        (Right html) = runPure $ writeHtml5String writerOptions fileContents''''

    -- Report missing IDs
    -- TODO: Warn on spurious-link spans
    unless (null failedIds) $
        hPutStrLn stderr $
           filePath ++ " contains invalid links: " ++ failedIdString
    TIO.writeFile outputPath html

  -- Create symlink to root node
  when (isJust rootNode) $ let (Just r) = rootNode in
    safeCreateLink (idToOutputName $ fileId r) (outputDir </> "root.html")

  buildIndex (M.elems idToData) (outputDir </> "index.html")

safeCreateLink :: FilePath -> FilePath -> IO ()
safeCreateLink linkTarget linkPath = do
  pathExists <- doesPathExist linkPath
  when pathExists $ removeFile linkPath
  createFileLink linkTarget linkPath

buildIndex :: [FileData] -> FilePath -> IO ()
buildIndex fileData outputPath = do
  TIO.writeFile outputPath indexHtml
    where (Right indexHtml) = runPure $ writeHtml5String def ir
          ir = Pandoc (Meta M.empty)
                      [Para [Link emptyAttr
                                  fileTitlePandoc
                                  (T.pack $ idToOutputName fileId, fileTitle)]
                      | FileData{..} <- fileData]


extractSubgraph :: M.Map FileId FileData -> FileId -> S.Set FileId
extractSubgraph idToData = add S.empty
    where add :: S.Set FileId -> FileId -> S.Set FileId
          add s id =
            if id `M.member` idToData && id `S.notMember` s
            then foldl' add s' (links ++ backlinks)
            else s
              where links = S.toList . fileLinkedIds $ idToData M.! id
                    backlinks = map fileId $ findBacklinks idToData id
                    s' = S.insert id s

fixLink :: M.Map FileId FileData -> Inline -> Writer [FileId] Inline
fixLink idToData link@(Link attrs text (href, title)) =
  case T.stripPrefix "id:" href of
    Just id ->
      let fileId = FileId id in
        if fileId `M.member` idToData
        then
          return $ Link (addClasses ["internal-link"])
                        text
                        (T.pack $ idToOutputName fileId, title)
        else do
          tell [fileId]
          return $ Span (addClasses ["broken-link", "internal-link"])
                        text

    Nothing -> return link

  where (htmlId, htmlClasses, htmlAttrs) = attrs
        addClasses newClasses = (htmlId, newClasses ++ htmlClasses, htmlAttrs)
fixLink _ inline = return inline

demoteHeaders :: Block -> Block
demoteHeaders (Header n a i) = Header (n+1) a i
demoteHeaders b = b

addTitle :: [Inline] -> Pandoc -> Pandoc
addTitle title (Pandoc m blocks) = Pandoc m $ titleHeader:blocks
  where titleHeader = Header 1 emptyAttr title

findBacklinks :: M.Map FileId FileData -> FileId -> [FileData]
findBacklinks idToFile id = M.foldl (\a v ->
    if id `S.member` fileLinkedIds v then v:a else a) [] idToFile

addBacklinks :: Pandoc -> [FileData] -> Pandoc
addBacklinks (Pandoc m blocks) linkingFiles = Pandoc m (blocks ++ [backlinks])
    where backlinks = Div ("", ["backlinks"], [])
                          [Para [Link emptyAttr
                                      fileTitlePandoc
                                      (T.pack $ idToOutputName fileId, fileTitle)]
                          | FileData{..} <- linkingFiles]

idToOutputName :: FileId -> FilePath
idToOutputName FileId{getIdText=id} = T.unpack id <.> "html"

fileToOutputPath :: FilePath -> FileData -> FilePath
fileToOutputPath outputDir d = outputDir </> idToOutputName (fileId d)

findFilePath :: M.Map FileId FilePath -> FileId -> Either FileId FilePath
findFilePath idPaths id = case idPaths M.!? id of Just p -> Right p
                                                  _ -> Left id

findLinkedIds :: Pandoc -> S.Set FileId
findLinkedIds = S.fromList . query findLinks
  where findLinks (Link _ _ (id, _)) = maybe [] ((:[]) . FileId)
                                             (T.stripPrefix "id:" id)
        findLinks _ = []

fileToSources :: FilePath -> IO [(FilePath, Text)]
fileToSources path = (:[]) . (path,) <$> TIO.readFile path

getFileId :: FilePath -> IO (Maybe FileId)
getFileId path = do
  -- We likely only want to read the first two lines of the file so we use lazy
  -- IO here.
  contents <- LT.lines <$> LazyTIO.readFile path
  let line = find (LT.isPrefixOf ":ID:") contents
  return $ FileId . LT.toStrict . LT.strip
         <$> (LT.stripPrefix (LT.pack ":ID:") =<< line)


findOrgFiles :: FilePath -> IO [FilePath]
findOrgFiles path = map (path </>)
                  . filter ("org" `isExtensionOf`)
                <$> listDirectory path

getFileId' :: FilePath -> ExceptIO FileId
getFileId' path = do
  id <- liftIO $ getFileId path
  maybe (throwError "Could not extract file ID") return id

getFilePandoc :: FilePath -> ExceptIO Pandoc
getFilePandoc path = do
  pandoc <- liftIO $ fileToSources path >>= (runIO . readOrg readerOptions)
  withExceptT show $ liftEither pandoc

getFileTitle :: Pandoc -> ExceptIO [Inline]
getFileTitle (Pandoc Meta{..} _) =
  maybe (throwError "Could not extract title") return title
  where title = extract <$> unMeta M.!? "title"
        extract (MetaInlines x) = x

getFileData :: FilePath -> ExceptIO FileData
getFileData path = do
  id <- getFileId' path
  pandoc <- getFilePandoc path
  titlePandoc <- getFileTitle pandoc
  return FileData { fileId = id
                  , filePath = path
                  , fileTitle = stringify titlePandoc
                  , fileTitlePandoc = titlePandoc
                  , fileContents = pandoc
                  , fileLinkedIds = findLinkedIds pandoc
                  }

emptyAttr = ("", [], [])
