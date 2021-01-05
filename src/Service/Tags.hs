{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Tags (
    updatePostTagList
  , updateFreeTagList
  , getTagList
  , getTagContents
  , getMstTagFromId
  ) where

import Import
import Data.Maybe
import qualified Database.Esqueleto as E
import DataTypes.HoubouType
import UrlParam.TagId
import Libs.Common
import Service.Common

getMstTagFromId ::
  TagId
  -> Handler (HResult MstTag)
getMstTagFromId tagId = runDB $ do
  let tagid = fromJust $ _unUrlTagId tagId
  tag <- get $ toTblMstTagKey tagid
  case tag of
    Just mstTag -> return $ Right $
      MstTag {
          unMstTagId = tagid
        , unMstTagName = tblMstTagName mstTag
        , unMstTagCreateTime = tblMstTagCreateTime mstTag
        , unMstTagUpdateTime = tblMstTagUpdateTime mstTag
        , unMstTagVersion = tblMstTagVersion mstTag
        }
    _ -> return $ Left ErrRecNotFound

getTagContents ::
  TagId
  -> Handler (HResult [TagContent])
getTagContents tagId = runDB $ do
  let tagid = fromJust $ _unUrlTagId tagId
      sql = "SELECT tid, rectype, tag_id, title, publish_date FROM " <>
            "((SELECT post_id AS tid, ? AS rectype, tag_id, t2.title, t2.publish_date FROM tbl_post_tag AS t1 " <>
            "LEFT JOIN tbl_post AS t2 ON t1.post_id=t2.id WHERE t2.status=? AND t2.publish_date IS NOT NULL)" <>
            "UNION" <>
            "(SELECT free_id AS tid, ? AS rectype, tag_id, t4.title, t4.publish_date FROM tbl_free_tag AS t3 " <>
            "LEFT JOIN tbl_free AS t4 ON t3.free_id=t4.id WHERE t4.status=? AND t4.publish_date IS NOT NULL)) " <>
            "AS tt WHERE tt.tag_id=? ORDER BY publish_date DESC" :: Text
  values <- getTagContRaw tagid sql
  return $ Right (map toTagCont values)

getTagContRaw :: MonadIO m =>
  Int64
  -> Text
  -> ReaderT E.SqlBackend m [
  ( E.Single Int64
  , E.Single Int
  , E.Single Int64
  , E.Single Text
  , E.Single UTCTime)]
getTagContRaw tagid sql = E.rawSql sql
    [
      E.PersistInt64 $ fromIntegral (fromEnum TypePost)
    , E.PersistInt64 $ fromIntegral (fromEnum Published)
    , E.PersistInt64 $ fromIntegral (fromEnum TypeFree)
    , E.PersistInt64 $ fromIntegral (fromEnum Published)
    , E.PersistInt64 tagid
    ]

getTagList ::
  Int ->
  Handler (HResult [MstTag])
getTagList lim = runDB $ do
  list <- selectList [] ([Desc TblMstTagId] ++ limit)
  return $ Right (map toMstTag list)
  where
    limit
      | lim > 0 = [ LimitTo lim ]
      | otherwise = []
       
updatePostTagList ::
  Int64
  -> TagStr
  -> Handler (HResult ())
updatePostTagList postId tags = do
  let tagList = splitTags tags
      key = toTblPostKey postId
  runDB $ do
    updateTagMaster tagList
    removePostTag key
    registerPostTag key tagList
  return $ Right ()

registerPostTag :: MonadIO m =>
  Key TblPost
  -> TagList
  -> ReaderT SqlBackend m ()
registerPostTag key tagList = do
  forM_ tagList (\tag ->
    do
      tk <- getBy $ UniTagName tag
      case tk of
        Just (Entity tagkey _) -> do
          _ <- insert $
                  TblPostTag {
                    tblPostTagTagId = tagkey
                  , tblPostTagPostId = key
                  }
          return ()
        Nothing -> error $ "registerPostTag: tag not exists master tbl_mst_tag tagname=" ++ unpack(tag)
      return ())
  return ()

removePostTag :: MonadIO m =>
  Key TblPost
  -> ReaderT SqlBackend m ()
removePostTag key = do
  deleteWhere [TblPostTagPostId ==. key]
  return ()

updateTagMaster :: MonadIO m =>
  TagList
  -> ReaderT SqlBackend m ()
updateTagMaster tagList = do
  now <- liftIO getTm
  forM_ tagList (\tag ->
    do
      t <- getBy $ UniTagName tag
      case t of
        Just _ -> return ()
        Nothing -> do
          _ <- insert $
                 TblMstTag {
                     tblMstTagName = tag
                   , tblMstTagCreateTime = now
                   , tblMstTagUpdateTime = now
                   , tblMstTagVersion = 1
                   }
          return ()
      return ())
  return ()

updateFreeTagList :: Int64
  -> TagStr
  -> Handler (HResult ())
updateFreeTagList freeId tags = do
  let tagList = splitTags tags
      key = toTblFreeKey freeId
  runDB $ do
    updateTagMaster tagList
    removeFreeTag key
    registerFreeTag key tagList
  return $ Right ()

removeFreeTag :: MonadIO m =>
  Key TblFree
  -> ReaderT SqlBackend m ()
removeFreeTag key = do
  deleteWhere [TblFreeTagFreeId ==. key]
  return ()

registerFreeTag :: MonadIO m =>
  Key TblFree
  -> TagList
  -> ReaderT SqlBackend m ()
registerFreeTag key tagList = do
  forM_ tagList (\tag ->
    do
      tk <- getBy $ UniTagName tag
      case tk of
        Just (Entity tagkey _) -> do
          _ <- insert $
                  TblFreeTag {
                    tblFreeTagTagId = tagkey
                  , tblFreeTagFreeId = key
                  }
          return ()
        Nothing -> error $
                     "registerFreeTag: tag not exists master tbl_mst_tag tagname="
                       ++ unpack(tag)
      return ())
  return ()
