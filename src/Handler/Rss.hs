{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Rss where

import Import
import Text.Blaze.Html (preEscapedText)
import qualified Prelude as P
import DataTypes.HoubouType
import Yesod.RssFeed
import Service.Feed
import Libs.CommonWidget
import Service.BlogSetting

getRssR :: Handler RepRss
getRssR = do
  master <- getYesod
  let setId = appBlogSettingId $ appSettings master
  setting <- getBlogSetting setId
  result <- getAtomList 30
  case result of
    Right atoms -> do
      retFeed setting atoms
    Left _ -> error "Error create rss feed error."

retFeed ::
  BlogSetting
  -> [HbAtom]
  -> Handler RepRss
retFeed _ [] = notFound
retFeed setting atoms = do
  let feed = initFeed setting atoms
  rssFeed $ feed
  
initFeed ::
  BlogSetting
  -> [HbAtom]
  -> Feed (Route App)
initFeed setting atoms = Feed {
    feedTitle = unBlogSettingBlogName setting
  , feedLinkSelf = HomeR
  , feedLinkHome = HomeR
  , feedAuthor = unBlogSettingBlogAuthor setting
  , feedDescription = preEscapedText $ unBlogSettingBlogDesc setting
  , feedLanguage = "ja"
  , feedUpdated = latest (atoms P.!! 0)
  , feedLogo = Nothing
  , feedEntries = initFeedEntry atoms
  }
  where latest atom = unHbAtomUpdateTime atom

initFeedEntry ::
  [HbAtom]
  -> [FeedEntry (Route App)]
initFeedEntry atoms =
  map (\atom ->
         FeedEntry {
            feedEntryLink = decideUrl atom
          , feedEntryUpdated = unHbAtomUpdateTime atom
          , feedEntryTitle = unHbAtomTitle atom
          , feedEntryContent = preEscapedText $ unHbAtomBody atom
          , feedEntryEnclosure = Nothing
          , feedEntryCategories = []
          }
         ) atoms

decideUrl ::
  HbAtom
  -> Route App
decideUrl atom = case pagetype of
  TypePost -> case slug of
    Just _ -> urlPostSlug slug urlpath
    Nothing -> urlPost tid
  TypeFree -> case slug of
    Just _ -> urlFreeSlug slug urlpath
    Nothing -> urlFree tid
  _ -> error "unreachable code."
  where
    pagetype = unHbAtomUrlType atom
    slug = unHbAtomSlug atom
    urlpath = unHbAtomUrlpath atom
    tid = unHbAtomId atom
