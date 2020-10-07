{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Access (
    getTatolTopAccess
  , getTatolAccess
  ) where

import Import
import DataTypes.HoubouType
import qualified Database.Esqueleto as E
import Service.Common

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
  , E.Single Int)]
getBlogContRaw sql = E.rawSql sql []
