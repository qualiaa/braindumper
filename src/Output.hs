{-# LANGUAGE RecordWildCards #-}
module Output
  ( idToOutputName
  , fileToOutputPath
  , safeCreateLink
  ) where

import Control.Monad.Extra (whenM)
import System.Directory ( createFileLink
                        , doesPathExist
                        , removeFile
                        )
import System.FilePath ((</>), (<.>))
import qualified Data.Text as T
import Types (FileId(..), FileData(..))


idToOutputName :: FileId -> FilePath
idToOutputName FileId{getIdText=id} = T.unpack id <.> "html"

fileToOutputPath :: FilePath -> FileData -> FilePath
fileToOutputPath outputDir d = outputDir </> idToOutputName (fileId d)

safeCreateLink :: FilePath -> FilePath -> IO ()
safeCreateLink linkTarget linkPath = do
  whenM (doesPathExist linkPath) $ removeFile linkPath
  createFileLink linkTarget linkPath
