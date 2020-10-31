{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Service.Post (
    getPostList
  , getPostFromId
  , getPublishPostFromSlug
  , getPublishPostFromId
  , registerPost
  , updatePost
  , updatePostStatus
  , getPublishPostedList
  , deletePost
  , accCntUp
  ) where

import Import
import qualified Prelude as P
import Data.Time.Clock
import Database.Persist.Sql (BackendKey(..))
import qualified Database.Esqueleto as E
import qualified Database.Esqueleto.Internal.Sql as E
import DataTypes.HoubouType
import UrlParam.PostId
import Libs.Common
import Service.Common

getPublishPostedList ::
  Handler (HResult [Post])
getPublishPostedList = runDB $ do
  now <- liftIO getTm
  let t = addUTCTime (-30 * nominalDay) now
      flt = [ TblPostStatus ==. fromEnum Published, TblPostPublishDate !=. Nothing ]
      opt = [ Desc TblPostPublishDate ]
  list <- selectList ([TblPostPublishDate >=. (Just t)] ++ flt) opt
  return $ Right (map toPost list)

getPostList ::
  PubViewStatus ->
  PageInfo ->
  Handler (HResult (Int, [PostList]))
getPostList blogViewStatus pageInfo = runDB $ do
  let pageNum = unPageNum pageInfo
      pagePerLine = fromIntegral $ unPagePerLine pageInfo
      blogWhere status tbl = case status of
        ViewAll -> E.val True
        ViewPublished -> tbl E.^. TblPostStatus E.==. E.val (fromEnum Published)
      countQuery = E.from $ \tblPost -> do
        E.where_ $ do blogWhere blogViewStatus tblPost
        return $ E.count(tblPost E.^. TblPostId)
      baseQuery = E.from $ \tblPost -> do
        let comCntTotal = E.subSelect $ E.from $ \tblComment -> do
              E.where_ $ tblComment E.^. TblCommentPostId E.==. tblPost E.^. TblPostId
              E.groupBy $ tblPost E.^. TblPostId
              return $ E.count(tblComment E.^. TblCommentPostId)
            comCntAprov = E.subSelect $ E.from $ \tblComment -> do
              E.where_ $
                tblComment E.^. TblCommentPostId E.==. tblPost E.^. TblPostId
                E.&&.
                tblComment E.^. TblCommentStatus E.==. E.val (fromEnum Approval)
              E.groupBy $ tblPost E.^. TblPostId
              return $ E.count(tblComment E.^. TblCommentPostId)
            rowNum = E.unsafeSqlValue "row_number() over(order by create_time desc)"
            postCnt = (E.subSelectMaybe $ E.from $ \tblPostAcc -> do
              E.where_ $ tblPostAcc E.^. TblPostAccPostId E.==. tblPost E.^. TblPostId
              E.groupBy $ tblPostAcc E.^. TblPostAccPostId
              return $ E.sum_(tblPostAcc E.^. TblPostAccViewCnt)) :: E.SqlExpr (E.Value (Maybe Int))
        E.where_ $ do blogWhere blogViewStatus tblPost
        E.orderBy [ E.desc $ tblPost E.^. TblPostPublishDate, E.desc $ tblPost E.^. TblPostCreateTime ]
        return
          (
            rowNum
          , tblPost E.^. TblPostId
          , tblPost E.^. TblPostStatus
          , tblPost E.^. TblPostTitle
          , tblPost E.^. TblPostSlug
          , tblPost E.^. TblPostUrlpath
          , tblPost E.^. TblPostPublishDate
          , tblPost E.^. TblPostCreateTime
          , tblPost E.^. TblPostVersion
          , comCntTotal
          , comCntAprov
          , postCnt
          )
      baseQueryPage = do
        r <- baseQuery
        E.offset (pagePerLine * (intToInt64 pageNum - 1))
        E.limit pagePerLine
        return r
  list <- E.select baseQueryPage
  totalCnt <- E.select countQuery
  return $ Right (E.unValue $ totalCnt P.!! 0, toPostList <$> list)

getPublishPostFromSlug ::
  Text
  -> Text
  -> Handler (HResult Post)
getPublishPostFromSlug urlpath slug = runDB $ do
  let flt = [
              TblPostStatus ==. fromEnum Published
            , TblPostPublishDate !=. Nothing
            , TblPostUrlpath ==. Just urlpath
            , TblPostSlug ==. Just slug
            ]
  p <- selectFirst flt []
  case p of
    Just entity -> return $ Right (toPost entity)
    Nothing -> return $ Left ErrRecNotFound

getPostFromId ::
  PostId
  -> Handler (HResult Post)
getPostFromId (PostId postId) = runDB $ do
  res <- case postId of
    (Just bid) -> do
      post <- get $ toTblPostKey bid
      case post of
        Just p -> return $ Right $
          Post {
              unPostId = bid
            , unPostTitle = tblPostTitle p
            , unPostContent = tblPostContent p
            , unPostTags = tblPostTags p
            , unPostHtml = tblPostHtml p
            , unPostSlug = tblPostSlug p
            , unPostUrlpath = tblPostUrlpath p
            , unPostInputType = tblPostInputType p
            , unPostStatus = tblPostStatus p
            , unPostPublishDate = tblPostPublishDate p
            , unPostDescription = tblPostDescription p
            , unPostKeywords = tblPostKeywords p
            , unPostRobots = tblPostRobots p
            , unPostCreateTime = tblPostCreateTime p
            , unPostUpdateTime = tblPostUpdateTime p
            , unPostAuthorId = unSqlBackendKey (unTblUserKey (tblPostAuthorId p))
            , unPostVersion = tblPostVersion p
            }
        _ -> return $ Left ErrRecNotFound
    Nothing -> return $ Left ErrParam
  return res  

getPublishPostFromId ::
  BlogSetting
  -> PostId
  -> Handler (HResult [Post])
getPublishPostFromId setting postId = runDB $ do
  let flt = [ TblPostStatus ==. fromEnum Published, TblPostPublishDate !=. Nothing ]
      opts = [ LimitTo $ unBlogSettingPostNum setting, Desc TblPostPublishDate ]
  list <- case _unPostId postId of
    Just pid -> do
      selectList ([TblPostId ==. (toTblPostKey pid)] ++ flt) opts
    _ -> do
      selectList flt opts
  return $ Right (map toPost list)

registerPost ::
  Key TblUser
  -> Post
  -> Handler (HResult Int64)
registerPost usrKey post = runDB $ do
  let status = unPostStatus post
  now <- liftIO getTm
  html <- liftIO $ convertMarkdownContents (unPostContent post) (unPostInputType post)
  TblPostKey (SqlBackendKey postId) <- insert $
    TblPost {
        tblPostTitle = unPostTitle post
      , tblPostContent = unPostContent post
      , tblPostInputType = unPostInputType post
      , tblPostHtml = html
      , tblPostSlug = unPostSlug post
      , tblPostUrlpath = unPostUrlpath post
      , tblPostTags = unPostTags post
      , tblPostStatus = status
      , tblPostPublishDate = initPublishDate status now
      , tblPostDescription = unPostDescription post
      , tblPostKeywords = unPostKeywords post
      , tblPostRobots = unPostRobots post
      , tblPostCreateTime = now
      , tblPostUpdateTime = now
      , tblPostAuthorId = usrKey
      , tblPostVersion = 1
      }
  return $ Right postId

updatePost ::
  Post
  -> Handler (HResult Int64)
updatePost post = runDB $ do
  let postKey = toTblPostKey $ unPostId post
  curRec <- get postKey
  res <- case curRec of
    Nothing -> return $ Left ErrRecNotFound
    Just record -> do
      let pver = unPostVersion post
          dver = tblPostVersion record
          urlpath = if isJust (unPostSlug post) && isNothing (tblPostUrlpath record) then
                      unPostUrlpath post
                    else
                      tblPostUrlpath record
          status = unPostStatus post
          pubDate = tblPostPublishDate record
      case checkVersion dver pver of
        False -> return $ Left ErrRecVersion
        True -> do
          now <- liftIO getTm
          html <- liftIO $ convertMarkdownContents (unPostContent post) (unPostInputType post)
          update postKey [
              TblPostTitle =. unPostTitle post
            , TblPostContent =. unPostContent post
            , TblPostTags =. unPostTags post
            , TblPostHtml =. html
            , TblPostSlug =. unPostSlug post
            , TblPostUrlpath =. urlpath
            , TblPostInputType =. unPostInputType post
            , TblPostStatus =. status
            , TblPostPublishDate =. updatePublishDate status pubDate now
            , TblPostDescription =. unPostDescription post
            , TblPostKeywords =. unPostKeywords post
            , TblPostRobots =. unPostRobots post
            , TblPostUpdateTime =. now
            , TblPostVersion +=. 1
            ]
          return $ Right (unPostId post)
  return res

updatePostStatus ::
  Key TblPost
  -> RecVersion
  -> Handler (HResultErrParam Int (PublishStatus, Int))
updatePostStatus postKey recver = runDB $ do
  now <- liftIO getTm
  curRec <- get postKey
  res <- case curRec of
    Nothing -> return $ Left (ErrRecNotFound, 0)
    Just post -> do
        let pver = unRecVersion recver
            dver = tblPostVersion post
            upPubDate = if isNothing (tblPostPublishDate post)
                        then [ TblPostPublishDate =. (Just now) ]
                        else mempty
            newStatus = if tblPostStatus post == (fromEnum Published)
                        then (fromEnum UnPublished)
                        else (fromEnum Published)
        case checkVersion dver pver of
          False -> return $ Left (ErrRecVersion, dver)
          True -> do
            update postKey $ [
              TblPostStatus =. newStatus
              , TblPostUpdateTime =. now
              , TblPostVersion +=. 1
              ] ++ upPubDate
            return $ Right ((toEnum newStatus), dver + 1)
  return res

deletePost ::
  Key TblPost
  -> RecVersion
  -> Handler (HResult Int64)
deletePost postKey recver = runDB $ do
  curRec <- get postKey
  res <- case curRec of
    Nothing -> return $ Left ErrRecNotFound
    Just post -> do
      let pver = unRecVersion recver
          dver = tblPostVersion post
          postId = unSqlBackendKey (unTblPostKey postKey)
      case checkVersion dver pver of
        False -> return $ Left ErrRecVersion
        True -> do
          delete postKey
          return $ Right postId
  return res

accCntUp :: PostId -> Handler (HResult ())
accCntUp (PostId postId) =
  case postId of
    (Just pid) -> accPostUp pid
    Nothing -> accHomeUp

accHomeUp :: Handler (HResult ())
accHomeUp = runDB $ do
  now <- liftIO getTm
  let nowTime = secZeroDatetime $ toGrecoFull now
  result <- selectFirst [ TblHomeAccAccTime ==. nowTime ] []
  case result of
    Just (Entity key _) -> do
      update key [
        TblHomeAccViewCnt +=. 1
        ]
    Nothing -> do
      _ <- insert $
        TblHomeAcc {
            tblHomeAccAccTime = nowTime
          , tblHomeAccViewCnt = 1
          }
      return ()
  return $ Right ()    

accPostUp ::
  Int64
  -> Handler (HResult ())
accPostUp pid = runDB $ do
  now <- liftIO getTm
  let nowTime = secZeroDatetime $ toGrecoFull now
      pkey = toTblPostKey pid
  result <- selectFirst [ TblPostAccPostId ==. pkey, TblPostAccAccTime ==. nowTime ] []
  case result of
    Just (Entity key _) -> do
      update key [
        TblPostAccViewCnt +=. 1
        ]
    Nothing -> do
      _ <- insert $
        TblPostAcc {
            tblPostAccPostId = pkey
          , tblPostAccAccTime = nowTime
          , tblPostAccViewCnt = 1
          }
      return ()
  return $ Right ()    
