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
  let sql = "SELECT T1.id, T1.slug, T1.urlpath, T1.update_time, T1.pagetype FROM ( " <>
            "  (SELECT id, slug, urlpath, update_time, ? AS pagetype FROM tbl_post " <>
            "   WHERE status=2 AND publish_date IS NOT NULL ORDER BY update_time DESC) " <>
            "UNION ALL " <>
            "  (SELECT id, slug, urlpath, update_time, ? AS pagetype FROM tbl_free " <>
            "   WHERE status=2 AND publish_date IS NOT NULL ORDER BY update_time DESC) " <>
            "UNION ALL " <>
            "  (SELECT T2.tag_id, NULL, NULL, T3.update_time, ? AS pagetype FROM ( " <>
            "    (SELECT tag_id FROM tbl_post_tag AS T4 LEFT JOIN tbl_post AS T5 ON " <>
            "     T4.post_id=T5.id WHERE T5.status=2 AND T5.publish_date IS NOT NULL)" <>
            "  UNION " <>
            "    (SELECT tag_id FROM tbl_free_tag AS T6 LEFT JOIN tbl_free AS T7 ON " <>
            "     T6.free_id=T7.id WHERE T7.status=2 AND T7.publish_date IS NOT NULL)" <>
            "  ) AS T2 LEFT JOIN tbl_mst_tag AS T3 ON T2.tag_id=T3.id ORDER BY T3.update_time)" <>
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
  , E.Single Int)]
getSiteUrlRaw offset sql = E.rawSql sql
  [
    E.PersistInt64 $ fromIntegral (fromEnum TypePost)
  , E.PersistInt64 $ fromIntegral (fromEnum TypeFree)
  , E.PersistInt64 $ fromIntegral (fromEnum TypeTag)
  , E.PersistInt64 $ fromIntegral offset
  ]
