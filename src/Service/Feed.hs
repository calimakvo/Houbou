{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Feed (
    getAtomList
  ) where

import Import
import DataTypes.HoubouType
import qualified Database.Esqueleto as E
import Service.Common

getAtomList ::
  Int
  -> Handler (HResult [HbAtom])
getAtomList limit = runDB $ do
  let sql = "SELECT T1.id, T1.slug, T1.urlpath, T1.update_time, T1.author_id, T1.pagetype, T1.title, T1.body " <>
        "FROM ((SELECT id, slug, urlpath, update_time, author_id, ? AS pagetype, title, " <>
        "(CASE WHEN input_type = 1 THEN html ELSE content END) AS body " <>
        "FROM tbl_post WHERE (status = 2) AND (publish_date IS NOT NULL) ORDER BY update_time DESC) " <>
        "UNION ALL " <>
        "(SELECT id, slug, urlpath, update_time, author_id, ? AS pagetype, title, " <>
        "(CASE WHEN input_type = 1 THEN html ELSE content END) AS body " <>
        "FROM tbl_free WHERE (status = 2) AND (publish_date IS NOT NULL) ORDER BY update_time DESC) " <>
        ") AS T1 ORDER BY T1.update_time DESC OFFSET 0 LIMIT ?" :: Text
  values <- getAtomRaw limit sql
  return $ Right (map toHbAtom values)

getAtomRaw :: MonadIO m =>
  Int
  -> Text
  -> ReaderT E.SqlBackend m [
  ( E.Single Int64
  , E.Single (Maybe Text)
  , E.Single (Maybe Text)
  , E.Single UTCTime
  , E.Single Int64
  , E.Single Int
  , E.Single Text
  , E.Single Text)]
getAtomRaw limit sql = E.rawSql sql
  [
    E.PersistInt64 $ fromIntegral (fromEnum TypePost)
  , E.PersistInt64 $ fromIntegral (fromEnum TypeFree)
  , E.PersistInt64 $ fromIntegral limit
  ]
