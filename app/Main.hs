{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Monad.Except
import Control.Monad.Extra (whenJust, whenM)
import Control.Monad.Writer
-- import Control.Monad.Trans.Maybe
-- import Control.Monad.Trans.Class (lift)
import Data.Default (def)
import Data.Either (isLeft, rights)
import Data.Either.Extra (maybeToEither, isLeft, rights)
import Data.Function ((&))
import Data.List (find, intercalate, foldl')
import Data.List.Extra (list, mconcatMap)
import Data.Text (Text)
import Options.Applicative (execParser)
import System.Directory ( createDirectoryIfMissing
                        , createFileLink
                        , doesPathExist
                        , listDirectory
                        , removeFile
                        )
import System.FilePath (isExtensionOf, equalFilePath, (</>), (<.>))
import System.IO  (hPutStrLn, stderr)
import Text.Pandoc.Class (runIO, runPure)
import Text.Pandoc.Definition ( docTitle
                              , Block(Header)
                              , Inline(..)
                              , Pandoc(..)
                              )
import Text.Pandoc.Builder (divWith, doc, link, para)
import Text.Pandoc.Walk (query, walk, walkM)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Readers (readOrg)
import Text.Pandoc.Writers (writeHtml5String)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as LazyTIO
import qualified Text.Pandoc.Builder as B
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

writerOptions = def
readerOptions = def --{readerStandalone=True}

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

  let idToData' = M.fromList $ zip (map fileId fileData) fileData
      idToData = maybe
          idToData'
          (M.restrictKeys idToData' . connectedNodes idToData')
          (fileId <$> rootNode)

  -- Report errors in file parsing
  forM_ (zip fileList fileParseResults) $ \(path, r) ->
    when (isLeft r) $ let (Left err) = r in hPutStrLn stderr (path ++ ": " ++ err)

  createDirectoryIfMissing False outputDir

  forM_ (M.elems idToData)  $ \currentFile@FileData{..} -> do
    let backlinks = findBacklinks idToData fileId

        (processedAST, failedIds) = fileContents
            & runWriter . walkM (fixLink idToData)
            . walk demoteHeaders
            . addBacklinks backlinks
            . addTitleHeader

        spuriousLinks = processedAST & flip query $ \case
            (Span (_, elem "spurious-link" -> True,
                      lookup "target" -> Just target)
             text) -> [target]
            _ -> []

        badLinksString = mconcatMap ("\n" <>) $
          map getIdText failedIds ++ spuriousLinks

        outputPath = fileToOutputPath outputDir currentFile
        (Right html) = runPure $ writeHtml5String writerOptions processedAST

    -- Report missing IDs
    unless (null failedIds && null spuriousLinks) $
        let err = mconcat [T.pack filePath, " contains invalid links: ",
                           badLinksString]
        in  TIO.hPutStrLn stderr err

    TIO.writeFile outputPath html

  -- Create symlink to root node
  whenJust rootNode $ \r ->
    let target = idToOutputName $ fileId r
        link = outputDir </> "root.html"
    in safeCreateLink target link

  buildIndex (M.elems idToData) $ outputDir </> "index.html"

safeCreateLink :: FilePath -> FilePath -> IO ()
safeCreateLink linkTarget linkPath = do
  whenM (doesPathExist linkPath) $ removeFile linkPath
  createFileLink linkTarget linkPath

buildIndex :: [FileData] -> FilePath -> IO ()
buildIndex fileData outputPath = do
  TIO.writeFile outputPath indexHtml
    where (Right indexHtml) = runPure $ writeHtml5String def ir
          ir = addTitleHeader . B.setTitle "All Pages" $ doc listing
          listing = mconcatMap (para . fileLink) fileData


connectedNodes :: M.Map FileId FileData -> FileId -> S.Set FileId
connectedNodes idToData = add S.empty
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

addTitleHeader :: Pandoc -> Pandoc
addTitleHeader (Pandoc m blocks) = Pandoc m blocks'
  where blocks' = titleHeader:blocks
        titleHeader = Header 1 emptyAttr $ docTitle m
        emptyAttr = ("", [], [])

findBacklinks :: M.Map FileId FileData -> FileId -> [FileData]
findBacklinks idToFile id = M.foldl (\a v ->
    if id `S.member` fileLinkedIds v then v:a else a) [] idToFile

addBacklinks :: [FileData] -> Pandoc -> Pandoc
addBacklinks linkingFiles (Pandoc m blocks) = Pandoc m (blocks ++ backlinks)
    where backlinks = B.toList $ divWith ("", ["backlinks"], []) listing
          listing = mconcatMap (para. fileLink) linkingFiles

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
getFileTitle (Pandoc m _) =
  list (throwError "Could not extract title") (\a b -> return $ a:b) $ docTitle m

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

fileLink :: FileData -> B.Inlines
fileLink FileData{fileTitle=linkTitle,..} = link url linkTitle linkText
  where url = T.pack $ idToOutputName fileId
        linkText = B.fromList fileTitlePandoc

whenNothing :: (Monad m) => Maybe a -> m () -> m (Maybe a)
whenNothing Nothing action = action >> return Nothing
whenNothing x _ = return x
