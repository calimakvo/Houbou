{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Free (
    getFreeList
  , getFreeFromId
  , getPublishFreeFromId
  , registerFree
  , updateFree
  , updateFreeStatus
  , deleteFree
  , accFreeCntUp
  ) where

import Import
import qualified Prelude as P
import Database.Persist.Sql (BackendKey(..))
import qualified Database.Esqueleto as E
import qualified Database.Esqueleto.Internal.Sql as E
import DataTypes.HoubouType
import UrlParam.FreeId
import Libs.Common
import Libs.Template
import Service.Common

getFreeList ::
  PubViewStatus ->
  PageInfo ->
  Handler (HResult (Int, [FreeList]))
getFreeList freeViewStatus pageInfo = runDB $ do
  let pageNum = unPageNum pageInfo
      pagePerLine = fromIntegral $ unPagePerLine pageInfo
      freeWhere status tbl = case status of
        ViewAll -> E.val True
        ViewPublished -> tbl E.^. TblFreeStatus E.==. E.val (fromEnum Published)
      countQuery = E.from $ \tblFree -> do
        E.where_ $ do freeWhere freeViewStatus tblFree
        return $ E.count(tblFree E.^. TblFreeId)
      baseQuery = E.from $ \tblFree -> do
        E.where_ $ do freeWhere freeViewStatus tblFree
        E.orderBy [ E.desc $ tblFree E.^. TblFreePublishDate, E.desc $ tblFree E.^. TblFreeCreateTime ]
        let rowNum = E.unsafeSqlValue "row_number() over(order by publish_date desc)"
            freeCnt = (E.subSelectMaybe $ E.from $ \tblFreeAcc -> do
              E.where_ $ tblFreeAcc E.^. TblFreeAccFreeId E.==. tblFree E.^. TblFreeId
              E.groupBy $ tblFreeAcc E.^. TblFreeAccFreeId
              return $ E.sum_(tblFreeAcc E.^. TblFreeAccViewCnt)) :: E.SqlExpr (E.Value (Maybe Int))
        return
          (
            rowNum
          , tblFree E.^. TblFreeId
          , tblFree E.^. TblFreeTitle
          , tblFree E.^. TblFreePublishDate
          , tblFree E.^. TblFreeStatus
          , tblFree E.^. TblFreeAuthorId
          , tblFree E.^. TblFreeCreateTime
          , tblFree E.^. TblFreeUpdateTime
          , tblFree E.^. TblFreeVersion
          , freeCnt
          )
      baseQueryPage = do
        r <- baseQuery
        E.offset (pagePerLine * (intToInt64 pageNum - 1))
        E.limit pagePerLine
        return r
  list <- E.select baseQueryPage
  totalCnt <- E.select countQuery
  return $ Right (E.unValue $ totalCnt P.!! 0, toFreeList <$> list)

getPublishFreeFromId ::
  BlogSetting
  -> FreeId
  -> Handler (HResult [Free])
getPublishFreeFromId _ freeId = runDB $ do
  let flt = [ TblFreeStatus ==. fromEnum Published, TblFreePublishDate !=. Nothing ]
  list <- case _unUrlFreeId freeId of
    Just fid -> do
      selectList ([TblFreeId ==. (toTblFreeKey fid)] ++ flt) []
    _ -> do
      selectList flt []
  return $ Right (map toFree list)

updateFree ::
  Free
  -> Handler (HResult Int64)
updateFree free = runDB $ do
  let freeKey = toTblFreeKey $ unFreeId free
  curRec <- get freeKey
  res <- case curRec of
    Nothing -> return $ Left ErrRecNotFound
    Just record -> do
      let pver = unFreeVersion free
          dver = tblFreeVersion record
      case checkVersion dver pver of
        False -> return $ Left ErrRecVersion
        _ -> do
          now <- liftIO getTm
          html <- if unFreeInputType free == (fromEnum ContTypeMarkdown) then
                      liftIO $ mdToHtml (unFreeContent free)
                  else
                      return Nothing
          update freeKey [
              TblFreeTitle =. unFreeTitle free
            , TblFreeContent =. unFreeContent free
            , TblFreeHtml =. html
            , TblFreeCss =. unFreeCss free
            , TblFreeTags =. unFreeTags free
            , TblFreeInputType =. unFreeInputType free
            , TblFreeUpdateTime =. now
            , TblFreeVersion +=. 1
            ]
          return $ Right (unFreeId free)
  return res

registerFree ::
  Key TblUser
  -> Free
  -> Handler (HResult Int64)
registerFree usrKey free = runDB $ do
  html <- if unFreeInputType free == (fromEnum ContTypeMarkdown) then
              liftIO $ mdToHtml (unFreeContent free)
          else
              return Nothing
  now <- liftIO getTm
  (TblFreeKey (SqlBackendKey freeId)) <- insert $
    TblFree {
        tblFreeTitle = unFreeTitle free
      , tblFreeContent = unFreeContent free
      , tblFreeHtml = html
      , tblFreeCss = unFreeCss free
      , tblFreeInputType = unFreeInputType free
      , tblFreeTags = unFreeTags free
      , tblFreeStatus = fromEnum UnPublished
      , tblFreePublishDate = Nothing
      , tblFreeCreateTime = now
      , tblFreeUpdateTime = now
      , tblFreeAuthorId = usrKey
      , tblFreeVersion = 1
      }
  return (Right freeId)

getFreeFromId ::
  FreeId
  -> Handler (HResult Free)
getFreeFromId (FreeId freeId) = runDB $ do
  case freeId of
    (Just fid) -> do
      free <- get $ toTblFreeKey fid
      case free of
        Just f -> return $ Right $
          Free {
              unFreeId = fid
            , unFreeTitle = tblFreeTitle f
            , unFreeContent = tblFreeContent f
            , unFreeHtml = tblFreeHtml f
            , unFreeCss = tblFreeCss f
            , unFreeInputType = tblFreeInputType f
            , unFreeTags = tblFreeTags f
            , unFreeStatus = tblFreeStatus f
            , unFreePublishDate = tblFreePublishDate f
            , unFreeCreateTime = tblFreeCreateTime f
            , unFreeUpdateTime = tblFreeUpdateTime f
            , unFreeAuthorId = unSqlBackendKey (unTblUserKey (tblFreeAuthorId f))
            , unFreeVersion = tblFreeVersion f
            }
        _ -> return $ Left ErrRecNotFound
    Nothing -> return $ Left ErrParam

updateFreeStatus ::
  Key TblFree
  -> RecVersion
  -> Handler (HResult PublishStatus)
updateFreeStatus freeKey recver = runDB $ do
  now <- liftIO getTm
  curRec <- get freeKey
  res <- case curRec of
    Nothing -> return $ Left ErrRecNotFound
    Just free -> do
      let pver = unRecVersion recver
          dver = tblFreeVersion free
          upPubDate = if isNothing (tblFreePublishDate free)
                      then [ TblFreePublishDate =. (Just now) ]
                      else mempty
          newStatus = if tblFreeStatus free == (fromEnum Published) then (fromEnum UnPublished)
                      else (fromEnum Published)
      case checkVersion dver pver of
        False -> return $ Left ErrRecVersion
        True -> do
          update freeKey $ [
              TblFreeStatus =. newStatus
            , TblFreeUpdateTime =. now
            , TblFreeVersion +=. 1
            ] ++ upPubDate
          return $ Right (toEnum newStatus)
  return res

deleteFree ::
  Key TblFree
  -> RecVersion
  -> Handler (HResult Int64)
deleteFree freeKey recver = runDB $ do
  curRec <- get freeKey
  res <- case curRec of
    Nothing -> return $ Left ErrRecNotFound
    Just free -> do
      let pver = unRecVersion recver
          dver = tblFreeVersion free
          freeId = unSqlBackendKey (unTblFreeKey freeKey)
      case checkVersion dver pver of
        False -> return $ Left ErrRecVersion
        True -> do
          delete freeKey
          return $ Right freeId
  return res

accFreeCntUp ::
  FreeId
  -> Handler (HResult ())
accFreeCntUp (FreeId freeId) = runDB $ do
  res <- case freeId of
    Nothing -> return ()
    Just (fid) -> do
      now <- liftIO getTm
      let nowTime = secZeroDatetime $ toGrecoFull now
          fkey = toTblFreeKey fid
      result <- selectFirst [ TblFreeAccFreeId ==. fkey, TblFreeAccAccTime ==. nowTime ] []
      case result of
        Just (Entity key _) -> do
          update key [
            TblFreeAccViewCnt +=. 1
            ]
          return ()
        Nothing -> do
          _ <- insert $
            TblFreeAcc {
                tblFreeAccFreeId = fkey
              , tblFreeAccAccTime = nowTime
              , tblFreeAccViewCnt = 1
              }
          return ()
  return $ Right res
