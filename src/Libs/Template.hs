{-# LANGUAGE OverloadedStrings #-}

module Libs.Template (
    createTemplate
  , mdToHtml
  , render
  ) where

import System.IO (IOMode(ReadMode), openFile, hGetContents)
import System.IO.Error (tryIOError)
import qualified Text.Ginger as G
import Text.Ginger.Html (htmlSource)
import qualified Data.HashMap.Strict as H
import Data.Aeson
import Data.Hashable
import Data.Text
import Text.Pandoc
import Libs.Common

createTemplate ::
  Maybe G.SourceName
  -> G.Source
  -> IO (Either G.ParserError (G.Template G.SourcePos))
createTemplate srcname src = G.parseGinger loadFileMay srcname src

mdToHtml ::
  Text
  -> IO (Maybe Text)
mdToHtml mdTxt = runIOorExplode $ readMarkdown defReaderOpt mdTxt >>=
  writeHtml5String defWriterOpt >>= return . toMaybeText

defReaderOpt ::
  ReaderOptions
defReaderOpt = def {
  readerExtensions = enableExtension Ext_smart pandocExtensions
  }

defWriterOpt ::
  WriterOptions
defWriterOpt = def {
  writerExtensions = enableExtension Ext_smart pandocExtensions
      -- import Text.Pandoc.Highlighting (pygments)
      -- , writerHighlightStyle = Just pygments
  }

render ::
  G.Template G.SourcePos
  -> H.HashMap G.VarName Value
  -> Text
render template contextMap =
  let contextLookup = flip scopeLookup contextMap
      context = G.makeContextHtml contextLookup
  in htmlSource $ G.runGinger context template

scopeLookup :: (Hashable k, Eq k, G.ToGVal m b) =>
  k
  -> H.HashMap k b
  -> G.GVal m
scopeLookup key context = G.toGVal $ H.lookup key context

loadFileMay ::
  FilePath
  -> IO (Maybe String)
loadFileMay fn =
  tryIOError (loadFile fn) >>= \e ->
    case e of
      Right contents -> return (Just contents)
      Left _ -> return Nothing
  where
    loadFile :: FilePath -> IO String
    loadFile fn' = openFile fn' ReadMode >>= hGetContents
