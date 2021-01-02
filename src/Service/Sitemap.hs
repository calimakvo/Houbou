{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Service.Sitemap (
      getSiteurlList
  ) where

import Import
import DataTypes.HoubouType
import qualified Database.Esqueleto as E
import Service.Common

getSiteurlList ::
  Int
  -> Handler (HResult [HbUrl])
getSiteurlList offset = runDB $ do
  let sql = "SELECT T1.id, T1.slug, T1.urlpath, T1.update_time, T1.author_id, T1.pagetype FROM ( " <>
            "  (SELECT id, slug, urlpath, update_time, author_id, ? AS pagetype FROM tbl_post " <>
            "   WHERE (status=2) AND (publish_date IS NOT NULL) ORDER BY update_time DESC) " <>
            "UNION ALL " <>
            "  (SELECT id, slug, urlpath, update_time, author_id, ? AS pagetype FROM tbl_free " <>
            "   WHERE (status='2') AND (publish_date IS NOT NULL) ORDER BY update_time DESC) " <>
            ") AS T1 ORDER BY T1.update_time DESC OFFSET ? LIMIT 50000" :: Text
  values <- getSiteUrlRaw offset sql
  return $ Right (map toHbUrl values)

getSiteUrlRaw :: MonadIO m =>
  Int
  -> Text
  -> ReaderT E.SqlBackend m [
  ( E.Single Int64
  , E.Single (Maybe Text)
  , E.Single (Maybe Text)
  , E.Single UTCTime
  , E.Single Int64
  , E.Single Int)]
getSiteUrlRaw offset sql = E.rawSql sql
  [
    E.PersistInt64 $ fromIntegral (fromEnum TypePost)
  , E.PersistInt64 $ fromIntegral (fromEnum TypeFree)
  , E.PersistInt64 $ fromIntegral offset
  ]
