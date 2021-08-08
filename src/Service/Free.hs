{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Free (
    getFreeList
  , getFreeFromId
  , getPublishFreeFromId
  , getPublishFreeFromSlug
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
import Service.Common

getFreeList ::
  SearchParam ->
  PageInfo ->
  Handler (HResult (Int, [FreeList]))
getFreeList sparam pageInfo = runDB $ do
  let pageNum = unPageNum pageInfo
      pagePerLine = fromIntegral $ unPagePerLine pageInfo
      freeWhere status tbl = case status of
        ViewAll ->  (tbl E.^. TblFreeStatus E.==. E.val (fromEnum Published)
                    E.||. tbl E.^. TblFreeStatus E.==. E.val (fromEnum UnPublished))
        ViewPublished -> tbl E.^. TblFreeStatus E.==. E.val (fromEnum Published)
        ViewDraft -> tbl E.^. TblFreeStatus E.==. E.val (fromEnum Draft)
      categoyWhere cateId tbl =
        case cateId of
          Just cid -> (tbl E.^. TblFreeCateId E.==. E.val (Just $ toTblCategoryKey $ intToInt64 cid))
          Nothing -> (E.val True)
      countQuery = E.from $ \tblFree -> do
        E.where_ $ do
          freeWhere (unSearchParamSearchType sparam) tblFree
          E.&&. (categoyWhere (unSearchParamCateId sparam) tblFree)
        return $ E.count(tblFree E.^. TblFreeId)
      baseQuery = E.from $ \tblFree -> do
        E.where_ $ do
          freeWhere (unSearchParamSearchType sparam) tblFree
          E.&&. (categoyWhere (unSearchParamCateId sparam) tblFree)
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
          , tblFree E.^. TblFreeSlug
          , tblFree E.^. TblFreeUrlpath
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
          urlpath = if isJust (tblFreeUrlpath record) then
                      tblFreeUrlpath record
                    else if unFreeStatus free == fromEnum Draft then
                      Nothing
                    else
                      unFreeUrlpath free
          status = unFreeStatus free
          pubDate = tblFreePublishDate record
      case checkVersion dver pver of
        False -> return $ Left ErrRecVersion
        _ -> do
          now <- liftIO getTm
          html <- liftIO $ convertMarkdownContents (unFreeContent free) (unFreeInputType free)
          update freeKey [
              TblFreeTitle =. unFreeTitle free
            , TblFreeContent =. unFreeContent free
            , TblFreeHtml =. html
            , TblFreeCss =. unFreeCss free
            , TblFreeSlug =. unFreeSlug free
            , TblFreeUrlpath =. urlpath
            , TblFreeTags =. unFreeTags free
            , TblFreeInputType =. unFreeInputType free
            , TblFreeStatus =. status
            , TblFreePublishDate =. updatePublishDate status pubDate now
            , TblFreeDescription =. unFreeDescription free
            , TblFreeKeywords =. unFreeKeywords free
            , TblFreeRobots =. unFreeRobots free
            , TblFreeOgImg =. unFreeOgImg free
            , TblFreeOgTitle =. unFreeOgTitle free
            , TblFreeOgUrl =. unFreeOgUrl free
            , TblFreeOgSiteName =. unFreeOgSiteName free
            , TblFreeOgDesc =. unFreeOgDesc free
            , TblFreeOgPageType =. unFreeOgPageType free
            , TblFreeCateId =. toTblCategoryKey <$> unFreeCateId free
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
  let status = unFreeStatus free
      urlpath =
        if unFreeStatus free == fromEnum Draft then
          Nothing
        else
          unFreeUrlpath free
  html <- liftIO $ convertMarkdownContents (unFreeContent free) (unFreeInputType free)
  now <- liftIO getTm
  (TblFreeKey (SqlBackendKey freeId)) <- insert $
    TblFree {
        tblFreeTitle = unFreeTitle free
      , tblFreeContent = unFreeContent free
      , tblFreeHtml = html
      , tblFreeCss = unFreeCss free
      , tblFreeSlug = unFreeSlug free
      , tblFreeUrlpath = urlpath
      , tblFreeInputType = unFreeInputType free
      , tblFreeTags = unFreeTags free
      , tblFreeStatus = status
      , tblFreePublishDate = initPublishDate status now
      , tblFreeDescription = unFreeDescription free
      , tblFreeKeywords = unFreeKeywords free
      , tblFreeRobots = unFreeRobots free
      , tblFreeOgImg = unFreeOgImg free
      , tblFreeOgTitle = unFreeOgTitle free
      , tblFreeOgUrl = unFreeOgUrl free
      , tblFreeOgSiteName = unFreeOgSiteName free
      , tblFreeOgDesc = unFreeOgDesc free
      , tblFreeOgPageType = unFreeOgPageType free
      , tblFreeCreateTime = now
      , tblFreeUpdateTime = now
      , tblFreeAuthorId = usrKey
      , tblFreeCateId = toTblCategoryKey <$> unFreeCateId free
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
            , unFreeSlug = tblFreeSlug f
            , unFreeUrlpath = tblFreeUrlpath f
            , unFreeInputType = tblFreeInputType f
            , unFreeTags = tblFreeTags f
            , unFreeStatus = tblFreeStatus f
            , unFreePublishDate = tblFreePublishDate f
            , unFreeDescription = tblFreeDescription f
            , unFreeKeywords = tblFreeKeywords f
            , unFreeRobots = tblFreeRobots f
            , unFreeOgImg = tblFreeOgImg f
            , unFreeOgTitle = tblFreeOgTitle f
            , unFreeOgUrl = tblFreeOgUrl f
            , unFreeOgSiteName = tblFreeOgSiteName f
            , unFreeOgDesc = tblFreeOgDesc f
            , unFreeOgPageType = tblFreeOgPageType f
            , unFreeCreateTime = tblFreeCreateTime f
            , unFreeUpdateTime = tblFreeUpdateTime f
            , unFreeAuthorId = fromTblUserKey $ tblFreeAuthorId f
            , unFreeCateId = fromTblCategoryKey <$> tblFreeCateId f
            , unFreeVersion = tblFreeVersion f
            }
        _ -> return $ Left ErrRecNotFound
    Nothing -> return $ Left ErrParam

getPublishFreeFromSlug ::
  Text
  -> Text
  -> Handler (HResult Free)
getPublishFreeFromSlug urlpath slug = runDB $ do
  let flt = [
              TblFreeStatus ==. fromEnum Published
            , TblFreePublishDate !=. Nothing
            , TblFreeUrlpath ==. Just urlpath
            , TblFreeSlug ==. Just slug
            ]
  p <- selectFirst flt []
  case p of
    Just entity -> return $ Right (toFree entity)
    Nothing -> return $ Left ErrRecNotFound

updateFreeStatus ::
  Key TblFree
  -> RecVersion
  -> Handler (HResultErrParam Int (PublishStatus, Int))
updateFreeStatus freeKey recver = runDB $ do
  now <- liftIO getTm
  curRec <- get freeKey
  res <- case curRec of
    Nothing -> return $ Left (ErrRecNotFound, 0)
    Just free -> do
      let pver = unRecVersion recver
          dver = tblFreeVersion free
          upPubDate = if isNothing (tblFreePublishDate free)
                      then [ TblFreePublishDate =. (Just now) ]
                      else mempty
          newStatus = if tblFreeStatus free == (fromEnum Published)
                      then (fromEnum UnPublished)
                      else (fromEnum Published)
      case checkVersion dver pver of
        False -> return $ Left (ErrRecVersion, dver)
        True -> do
          update freeKey $ [
            TblFreeStatus =. newStatus
            , TblFreeUpdateTime =. now
            , TblFreeVersion +=. 1
            ] ++ upPubDate
          return $ Right ((toEnum newStatus), dver + 1)
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
