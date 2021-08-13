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
                      TypeTag -> siteMapTag url
                      TypeCate -> siteMapCate url) urls
    Left _ -> error "Error create sitemap."

siteMapUrl ::
  HbUrl
  -> SlugUrl
  -> IdUrl
  -> SitemapUrl (Route App)
siteMapUrl (HbUrl tid _ slug urlpath upt) slugRoute idRoute =
  case slug of
    Just _ -> SitemapUrl (slugRoute slug urlpath) (Just upt) Nothing Nothing
    Nothing -> SitemapUrl (idRoute tid) (Just upt) Nothing Nothing

siteMapTag ::
  HbUrl
  -> SitemapUrl (Route App)
siteMapTag (HbUrl tid _ _ _ upt) = SitemapUrl (urlTagList tid) (Just upt) Nothing Nothing

siteMapCate ::
  HbUrl
  -> SitemapUrl (Route App)
siteMapCate (HbUrl tid _ _ _ upt) = SitemapUrl (urlCateList tid) (Just upt) Nothing Nothing
