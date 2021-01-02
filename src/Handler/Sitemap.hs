module Handler.Sitemap where

import Import
import Yesod.Sitemap
import DataTypes.HoubouType
import Libs.CommonWidget
import Service.Sitemap

getSitemapR :: Handler TypedContent
getSitemapR = do
  result <- getSiteurlList 0
  case result of
    Right urls -> sitemapList $
      Import.map (\url ->
                    case unHbUrlType url of
                      TypePost -> siteMapPost url
                      TypeFree -> siteMapFree url) urls
    Left _ -> error "Error create sitemap."

siteMapPost ::
  HbUrl -> SitemapUrl (Route App)
siteMapPost url = case unHbUrlSlug url of
  Just _ -> SitemapUrl (urlPostSlug slug urlpath) (Just upt) Nothing Nothing
  Nothing -> SitemapUrl (urlPost tid) (Just upt) Nothing Nothing
  where tid = unHbUrlId url
        slug = unHbUrlSlug url
        urlpath = unHbUrlUrlpath url
        upt = unHbUrlUpdateTime url

siteMapFree ::
  HbUrl -> SitemapUrl (Route App)
siteMapFree url = case unHbUrlSlug url of
  Just _ -> SitemapUrl (urlFreeSlug slug urlpath) (Just upt) Nothing Nothing
  Nothing -> SitemapUrl (urlFree tid) (Just upt) Nothing Nothing
  where tid = unHbUrlId url
        slug = unHbUrlSlug url
        urlpath = unHbUrlUrlpath url
        upt = unHbUrlUpdateTime url
