module Handler.Sitemap where

import Import
import Yesod.Sitemap
import DataTypes.HoubouType
import Libs.CommonWidget
import Service.Sitemap

type SlugUrl = Maybe Text -> Maybe Text -> Route App
type IdUrl = Int64 -> Route App

getSitemapR :: Handler TypedContent
getSitemapR = do
  result <- getSiteurlList 0
  case result of
    Right urls -> sitemapList $
      Import.map (\url ->
                    case unHbUrlType url of
                      TypePost -> siteMapUrl url urlPostSlug urlPost
                      TypeFree -> siteMapUrl url urlFreeSlug urlFree
                      TypeTag -> siteMapTag url) urls
    Left _ -> error "Error create sitemap."

siteMapUrl ::
  HbUrl
  -> SlugUrl
  -> IdUrl
  -> SitemapUrl (Route App)
siteMapUrl url slugRoute idRoute =
  case slug of
    Just _ -> SitemapUrl (slugRoute slug urlpath) (Just upt) Nothing Nothing
    Nothing -> SitemapUrl (idRoute tid) (Just upt) Nothing Nothing
  where tid = unHbUrlId url
        slug = unHbUrlSlug url
        urlpath = unHbUrlUrlpath url
        upt = unHbUrlUpdateTime url

siteMapTag ::
  HbUrl -> SitemapUrl (Route App)
siteMapTag url = SitemapUrl (urlTagList tid) (Just upt) Nothing Nothing
  where tid = unHbUrlId url
        upt = unHbUrlUpdateTime url
