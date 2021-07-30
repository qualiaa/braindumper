module Types
  ( FileId(..)
  , FileData(..)
  ) where

import Data.Set (Set)
import Data.Text (Text)
import Text.Pandoc.Definition (Inline(..), Pandoc(..))

newtype FileId = FileId { getIdText :: Text } deriving (Eq, Ord, Show)

data FileData = FileData
  { fileId :: FileId
  , filePath :: FilePath
  , fileTitle :: Text
  , fileTitlePandoc :: [Inline]
  , fileContents :: Pandoc
  , fileLinkedIds :: Set FileId
  } deriving (Eq, Show)
