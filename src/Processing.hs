{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Processing
  ( addTitleHeader
  , addBacklinks
  , buildIndex
  , demoteHeaders
  , fixLinks
  , processFileContents
  ) where


import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Default (def)
import Data.List.Extra (mconcatMap)
import Text.Pandoc.Builder (divWith, doc, link, para)
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Definition ( docTitle
                              , Block(Header)
                              , Inline(..)
                              , Pandoc(..)
                              )
import Text.Pandoc.Walk (walk, walkM)
import Text.Pandoc.Writers (writeHtml5String)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Pandoc.Builder as B
import Output (idToOutputName)
import Types (FileId(..), FileData(..))

fixLinks :: M.Map FileId FileData -> Inline -> Writer [FileId] Inline
fixLinks idToData link@(Link attrs text (href, title)) =
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
fixLinks _ inline = return inline

demoteHeaders :: Block -> Block
demoteHeaders (Header n a i) = Header (n+1) a i
demoteHeaders b = b

addTitleHeader :: Pandoc -> Pandoc
addTitleHeader (Pandoc m blocks) = Pandoc m blocks'
  where blocks' = titleHeader:blocks
        titleHeader = Header 1 emptyAttr $ docTitle m
        emptyAttr = ("", [], [])

addBacklinks :: [FileData] -> Pandoc -> Pandoc
addBacklinks linkingFiles (Pandoc m blocks) = Pandoc m (blocks ++ backlinks)
    where backlinks = B.toList $ divWith ("", ["backlinks"], []) listing
          listing = mconcatMap (para. fileLink) linkingFiles

buildIndex :: [FileData] -> FilePath -> IO ()
buildIndex fileData outputPath = do
  TIO.writeFile outputPath indexHtml
    where (Right indexHtml) = runPure $ writeHtml5String def ir
          ir = addTitleHeader . B.setTitle "All Pages" $ doc listing
          listing = mconcatMap (para . fileLink) fileData

fileLink :: FileData -> B.Inlines
fileLink FileData{fileTitle=linkTitle,..} = link url linkTitle linkText
  where url = T.pack $ idToOutputName fileId
        linkText = B.fromList fileTitlePandoc

processFileContents :: M.Map FileId FileData
                    -> [FileData]
                    -> Pandoc
                    -> (Pandoc, [FileId])
processFileContents flatGraph fileBacklinks = runWriter
     . walkM (fixLinks flatGraph)
     . walk demoteHeaders
     . addBacklinks fileBacklinks
     . addTitleHeader
