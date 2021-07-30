{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Input
  ( findOrgFiles
  , getFileData
  ) where

import Control.Monad.Except (ExceptT, liftEither, withExceptT, liftIO, throwError)
import Data.Default (def)
import Data.List (find)
import Data.List.Extra (list)
import Data.Text (Text)
import System.Directory (listDirectory)
import System.FilePath (isExtensionOf, (</>))
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Definition (docTitle, Inline(..), Pandoc(..))
import Text.Pandoc.Readers (readOrg)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (query)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LazyTIO
import Types

type ExceptIO = ExceptT String IO

readerOptions = def --{readerStandalone=True}

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

findOrgFiles :: FilePath -> IO [FilePath]
findOrgFiles path = map (path </>)
                  . filter ("org" `isExtensionOf`)
                <$> listDirectory path


getFileId :: FilePath -> IO (Maybe FileId)
getFileId path = do
  -- We likely only want to read the first two lines of the file so we use lazy
  -- IO here.
  contents <- LT.lines <$> LazyTIO.readFile path
  let line = find (LT.isPrefixOf ":ID:") contents
  return $ FileId . LT.toStrict . LT.strip
         <$> (LT.stripPrefix (LT.pack ":ID:") =<< line)

fileToSources :: FilePath -> IO [(FilePath, Text)]
fileToSources path = (:[]) . (path,) <$> TIO.readFile path

findLinkedIds :: Pandoc -> S.Set FileId
findLinkedIds = S.fromList . query findLinks
  where findLinks (Link _ _ (id, _)) = maybe [] ((:[]) . FileId)
                                             (T.stripPrefix "id:" id)
        findLinks _ = []
