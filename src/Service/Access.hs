{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Access (
    getTatolTopAccess
  , getTatolAccess
  , getAccessDay
  ) where

import Import
import qualified Database.Esqueleto as E
import DataTypes.HoubouType
import Service.Common

getAccessDay ::
  Maybe UTCTime
  -> Maybe UTCTime
  -> Handler (Int, [AccDay])
getAccessDay from to = do
  let fwhr = if isJust from == True then [" timezone('JST', acc_time) >= ? "] else []
      twhr = if isJust to == True then [" timezone('JST', acc_time) <= ? "] else []
      whr = intercalate " AND " $ fwhr ++ twhr
      sql = "SELECT row_number() OVER (), tid, rectype, cnt, title, slug, urlpath FROM " <>
            "((SELECT post_id as tid, 1 as rectype, sum(view_cnt) AS cnt, title, slug, urlpath " <>
            "FROM tbl_post_acc AS T1 LEFT JOIN tbl_post AS T2 ON T1.post_id=T2.id " <>
            "WHERE " <> whr <> "GROUP BY post_id, title, slug, urlpath) " <>
            "UNION " <>
            "(SELECT free_id as tid, 2 as rectype, sum(view_cnt) AS cnt, title, slug, urlpath " <>
            "FROM tbl_free_acc AS T3 LEFT JOIN tbl_free AS T4 ON T3.free_id=T4.id " <>
            "WHERE " <> whr <> "GROUP BY free_id,title,slug,urlpath)) AS T5 ORDER BY T5.cnt DESC" :: Text
  runDB $ do
    values <- getBlogAccDayRaw sql from to
    let xs = map toAccDay values
        totalCnt = foldr (\ac -> (+unAccDayAccCnt ac)) 0 xs
    return $ (totalCnt, xs)

getTatolTopAccess ::
  Handler (HResult [BlogAccess])
getTatolTopAccess = runDB $ do
  let sql = "SELECT t1.dates as dates, COALESCE(t2.tcnt, 0) AS tcnt FROM " <>
            "(SELECT current_date + s.a AS dates FROM generate_series(-30,0) AS s(a)) AS t1 LEFT JOIN " <>
            "(SELECT CAST(date_trunc('day',acc_time) AS DATE) AS at, SUM(hp.view_cnt) AS tcnt FROM tbl_home_acc AS hp " <>
            " GROUP BY at) AS t2 ON t2.at=t1.dates " <>
            "ORDER BY dates ASC" :: Text
  values <- getBlogContRaw sql
  return $ Right (map toBlogAccess values)

getTatolAccess :: Handler (HResult [BlogAccess])
getTatolAccess = runDB $ do
  let sql = "SELECT t1.dates AS dates, COALESCE(t2.tcnt, 0) AS tcnt FROM " <>
            "(SELECT current_date + s.a AS dates FROM generate_series(-30,0) AS s(a)) AS t1  LEFT JOIN " <>
            "(SELECT acc_time AS at, sum(view_cnt) AS tcnt FROM (" <>
            "(SELECT CAST(date_trunc('day', acc_time) AS DATE) AS acc_time, tp.view_cnt FROM tbl_post_acc AS tp) " <>
            "UNION ALL " <>
            "(SELECT CAST(date_trunc('day', acc_time) as DATE) AS acc_time, tf.view_cnt FROM tbl_free_acc AS tf )" <>
            ") AS t2 GROUP BY acc_time) AS t2 ON t2.at = t1.dates ORDER BY dates ASC"
  values <- getBlogContRaw sql
  return $ Right (map toBlogAccess values)

getBlogContRaw :: MonadIO m =>
  Text
  -> ReaderT E.SqlBackend m [
  ( E.Single Day
  , E.Single Int )]
getBlogContRaw sql = E.rawSql sql []

getBlogAccDayRaw :: MonadIO m =>
  Text
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> ReaderT E.SqlBackend m [
  ( E.Single Int
  , E.Single Int64
  , E.Single Int
  , E.Single Int
  , E.Single Text
  , E.Single (Maybe Text)
  , E.Single (Maybe Text)
  )]
getBlogAccDayRaw sql from to =
  E.rawSql sql $ (w from) ++ (w to) ++ (w from) ++ (w to)
    where
      w t = case t of
              (Just tm) -> [PersistUTCTime tm]
              Nothing -> []
