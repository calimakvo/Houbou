{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Service.Common (
    toPostList
  , toFreeList
  , toTagCont
  , toBlogAccess
  , toTblPostKey
  , toTblFrameKey
  , toTblUserKey
  , toTblFreeKey
  , toTblMstUserPermKey
  , toFrame
  , toFrameList
  , toFreeFrame
  , toFree
  , toBlogAccessList
  , toFreeFrameList
  , toTblFreeFrameKey
  , toTblMstMimeTypeKey
  , toTblMediaKey
  , toTblHomeAccKey
  , toTblPostAccKey
  , toMedia
  , toMstTag
  , toTblMstTagKey
  , fromTblFreeKey
  , fromTblMstMimeTypeKey
  , fromTblMstUserPermKey
  , fromTblFrameKey
  , fromTblPostKey
  , fromTblFreeFrameKey
  , fromTblMediaKey
  , fromTblBlogSettingKey
  , fromTblHomeAccKey
  , fromTblPostAccKey
  , fromTblMstTagKey
  , fromTblPostTagKey
  , fromTblFreeTagKey
  , checkVersion
  , toBlogSetting
  , toUserList
  , toUser
  , toPost
  , fromTblUserKey
  , getTblMstUserPerm
  , toMstUserPerm
  , toMediaList
  , toMstMimeType
  , eitherToMaybe
  , eitherToBool
  , eitherToList
  , eitherToTouple
  , initPublishDate
  , updatePublishDate
  , convertMarkdownContents
  ) where

import Import
import Database.Persist.Sql (BackendKey(..))
import qualified Database.Esqueleto as E
import DataTypes.HoubouType
import Libs.Template

toFreeList ::
  (
    E.Value Int
  , E.Value (Key TblFree)
  , E.Value Text
  , E.Value (Maybe Text)
  , E.Value (Maybe Text)
  , E.Value (Maybe UTCTime)
  , E.Value Int
  , E.Value (Key TblUser)
  , E.Value UTCTime
  , E.Value UTCTime
  , E.Value Int
  , E.Value (Maybe Int)
  ) -> FreeList
toFreeList (
    E.Value rownum
  , E.Value key
  , E.Value title
  , E.Value slug
  , E.Value urlpath
  , E.Value pubDate
  , E.Value status
  , E.Value author
  , E.Value ctime
  , E.Value utime
  , E.Value version
  , E.Value viewCnt) 
  = FreeList rownum (fromTblFreeKey key) title
             slug urlpath pubDate
             status (fromTblUserKey author) ctime
             utime version (countMaybe viewCnt)

toFrameList ::
  (
    E.Value Int
  , E.Value (Key TblFrame)
  , E.Value (Maybe Text)
  , E.Value Bool
  , E.Value (Maybe UTCTime)
  , E.Value UTCTime
  , E.Value UTCTime
  , E.Value Int
  ) -> FrameList
toFrameList (
    E.Value rownum
  , E.Value key
  , E.Value fname
  , E.Value vflag
  , E.Value pubDate
  , E.Value ctime
  , E.Value utime
  , E.Value version) 
  = FrameList rownum (fromTblFrameKey key) fname vflag pubDate ctime utime version

toFreeFrameList ::
  (
    E.Value Int
  , E.Value (Key TblFreeFrame)
  , E.Value (Maybe Text)
  , E.Value Bool
  , E.Value (Maybe UTCTime)
  , E.Value UTCTime
  , E.Value UTCTime
  , E.Value Int
  ) -> FreeFrameList
toFreeFrameList
  (
    E.Value rownum
  , E.Value key
  , E.Value fname
  , E.Value vflag
  , E.Value pubDate
  , E.Value ctime
  , E.Value utime
  , E.Value version
  )
  = FreeFrameList rownum (fromTblFreeFrameKey key) fname vflag pubDate ctime utime version

toPostList ::
  (
    E.Value Int
  , E.Value (Key TblPost)
  , E.Value Int
  , E.Value Text
  , E.Value (Maybe Text)
  , E.Value (Maybe Text)
  , E.Value (Maybe UTCTime)
  , E.Value UTCTime
  , E.Value Int
  , E.Value (Maybe Int)
  , E.Value (Maybe Int)
  , E.Value (Maybe Int)
  ) -> PostList
toPostList
  (
    E.Value rownum
  , E.Value key
  , E.Value status
  , E.Value title
  , E.Value slug
  , E.Value utlpath
  , E.Value pubdate
  , E.Value ctime
  , E.Value version
  , E.Value comCntTotal
  , E.Value comCntAprov
  , E.Value postCnt
  )
  = PostList rownum (fromTblPostKey key) status
      title slug utlpath
      pubdate ctime version
      (countMaybe comCntTotal)
      (countMaybe comCntAprov)
      (countMaybe postCnt)

toBlogAccessList ::
  (
    E.Value UTCTime
  , E.Value Int
  ) -> BlogAccess
toBlogAccessList
  (
    E.Value (UTCTime day _)
  , E.Value cnt
  ) = BlogAccess day cnt

toTblPostKey :: Int64 -> Key TblPost
toTblPostKey = TblPostKey . SqlBackendKey

toTblFrameKey :: Int64 -> Key TblFrame
toTblFrameKey = TblFrameKey . SqlBackendKey 

toTblFreeFrameKey :: Int64 -> Key TblFreeFrame
toTblFreeFrameKey = TblFreeFrameKey . SqlBackendKey

toTblMediaKey :: Int64 -> Key TblMedia
toTblMediaKey = TblMediaKey . SqlBackendKey 

toTblUserKey :: Int64 -> Key TblUser
toTblUserKey = TblUserKey . SqlBackendKey

toTblHomeAccKey :: Int64 -> Key TblHomeAcc
toTblHomeAccKey = TblHomeAccKey . SqlBackendKey

toTblPostAccKey :: Int64 -> Key TblPostAcc
toTblPostAccKey = TblPostAccKey . SqlBackendKey

toTblMstTagKey :: Int64 -> Key TblMstTag
toTblMstTagKey = TblMstTagKey . SqlBackendKey

fromTblFreeTagKey :: Key TblFreeTag -> Int64
fromTblFreeTagKey = unSqlBackendKey . unTblFreeTagKey

fromTblPostTagKey :: Key TblPostTag -> Int64
fromTblPostTagKey = unSqlBackendKey . unTblPostTagKey

fromTblMstTagKey :: Key TblMstTag -> Int64
fromTblMstTagKey = unSqlBackendKey . unTblMstTagKey

fromTblMediaKey :: Key TblMedia -> Int64
fromTblMediaKey = unSqlBackendKey . unTblMediaKey

fromTblFrameKey :: Key TblFrame -> Int64
fromTblFrameKey = unSqlBackendKey . unTblFrameKey

fromTblUserKey :: Key TblUser -> Int64
fromTblUserKey = unSqlBackendKey . unTblUserKey

fromTblPostKey :: Key TblPost -> Int64
fromTblPostKey = unSqlBackendKey . unTblPostKey

fromTblFreeFrameKey :: Key TblFreeFrame -> Int64
fromTblFreeFrameKey = unSqlBackendKey . unTblFreeFrameKey

toTblFreeKey :: Int64 -> Key TblFree
toTblFreeKey = TblFreeKey . SqlBackendKey 

fromTblFreeKey :: Key TblFree -> Int64
fromTblFreeKey = unSqlBackendKey . unTblFreeKey

toTblMstUserPermKey :: Int64 -> Key TblMstUserPerm
toTblMstUserPermKey = TblMstUserPermKey . SqlBackendKey

fromTblMstUserPermKey :: Key TblMstUserPerm -> Int64
fromTblMstUserPermKey = unSqlBackendKey . unTblMstUserPermKey

toTblMstMimeTypeKey :: Int64 -> Key TblMstMimeType
toTblMstMimeTypeKey = TblMstMimeTypeKey . SqlBackendKey

fromTblMstMimeTypeKey :: Key TblMstMimeType -> Int64
fromTblMstMimeTypeKey = unSqlBackendKey . unTblMstMimeTypeKey

fromTblBlogSettingKey :: Key TblBlogSetting -> Int64
fromTblBlogSettingKey = unSqlBackendKey . unTblBlogSettingKey

fromTblHomeAccKey :: Key TblHomeAcc -> Int64
fromTblHomeAccKey = unSqlBackendKey . unTblHomeAccKey

fromTblPostAccKey :: Key TblPostAcc -> Int64
fromTblPostAccKey = unSqlBackendKey . unTblPostAccKey

countMaybe ::
  Maybe Int
  -> Int
countMaybe = fromMaybe 0

toMstMimeType ::
  Entity TblMstMimeType
  -> MstMimeType
toMstMimeType (Entity key entity) = MstMimeType {
    unMstMimeTypeId = fromTblMstMimeTypeKey key
  , unMstMimeTypeExtension = tblMstMimeTypeExtension entity
  , unMstMimeTypeMimeType = tblMstMimeTypeMimeType entity
  , unMstMimeTypeCreateTime = tblMstMimeTypeCreateTime entity
  , unMstMimeTypeUpdateTime = tblMstMimeTypeUpdateTime entity
  , unMstMimeTypeVersion = tblMstMimeTypeVersion entity
  , unMstMimeTypeDeleteFlag = tblMstMimeTypeDeleteFlag entity
  }

toFrame ::
  Entity TblFrame
  -> Frame
toFrame (Entity key entity) = Frame {
    unFrameId = fromTblFrameKey key
  , unFrameName = tblFrameName entity
  , unFrameHtml = tblFrameHtml entity
  , unFrameCss = tblFrameCss entity
  , unFrameValidFlag = tblFrameValidFlag entity
  , unFramePublishDate = tblFramePublishDate entity
  , unFrameCreateTime = tblFrameCreateTime entity
  , unFrameUpdateTime = tblFrameUpdateTime entity
  , unFrameVersion = tblFrameVersion entity
  }

toFree ::
  Entity TblFree
  -> Free
toFree (Entity key entity) = Free {
    unFreeId = fromTblFreeKey key
  , unFreeTitle = tblFreeTitle entity
  , unFreeContent = tblFreeContent entity
  , unFreeHtml = tblFreeHtml entity
  , unFreeCss = tblFreeCss entity
  , unFreeSlug = tblFreeSlug entity
  , unFreeUrlpath = tblFreeUrlpath entity
  , unFreeInputType = tblFreeInputType entity
  , unFreeTags = tblFreeTags entity
  , unFreeStatus = tblFreeStatus entity
  , unFreePublishDate = tblFreePublishDate entity
  , unFreeDescription = tblFreeDescription entity
  , unFreeKeywords = tblFreeKeywords entity
  , unFreeRobots = tblFreeRobots entity
  , unFreeOgImg = tblFreeOgImg entity
  , unFreeOgTitle = tblFreeOgTitle entity
  , unFreeOgUrl = tblFreeOgUrl entity
  , unFreeOgSiteName = tblFreeOgSiteName entity
  , unFreeOgDesc = tblFreeOgDesc entity
  , unFreeOgPageType = tblFreeOgPageType entity
  , unFreeCreateTime = tblFreeCreateTime entity
  , unFreeUpdateTime = tblFreeUpdateTime entity
  , unFreeAuthorId = fromTblUserKey $ tblFreeAuthorId entity
  , unFreeVersion = tblFreeVersion entity
  }

toFreeFrame ::
  Entity TblFreeFrame
  -> FreeFrame
toFreeFrame (Entity key entity) = FreeFrame {
    unFreeFrameId = fromTblFreeFrameKey key
  , unFreeFrameName = tblFreeFrameName entity
  , unFreeFrameHtml = tblFreeFrameHtml entity
  , unFreeFrameCss = tblFreeFrameCss entity
  , unFreeFrameValidFlag = tblFreeFrameValidFlag entity
  , unFreeFramePublishDate = tblFreeFramePublishDate entity
  , unFreeFrameCreateTime = tblFreeFrameCreateTime entity
  , unFreeFrameUpdateTime = tblFreeFrameUpdateTime entity
  , unFreeFrameVersion = tblFreeFrameVersion entity
}

toBlogSetting ::
  Entity TblBlogSetting
  -> BlogSetting
toBlogSetting (Entity key entity) = BlogSetting {
    unBlogSettingId = fromTblBlogSettingKey key
  , unBlogSettingIdent =  tblBlogSettingIdent entity
  , unBlogSettingBlogName =  tblBlogSettingBlogName entity
  , unBlogSettingBlogUrl = tblBlogSettingBlogUrl entity
  , unBlogSettingPostNum = tblBlogSettingPostNum entity
  , unBlogSettingMediaUrl = tblBlogSettingMediaUrl entity
  , unBlogSettingMediaDir = tblBlogSettingMediaDir entity
  , unBlogSettingUploadSize = tblBlogSettingUploadSize entity
  , unBlogSettingSessionTimeout = tblBlogSettingSessionTimeout entity
  , unBlogSettingAdstxt = tblBlogSettingAdstxt entity
  , unBlogSettingCreateTime = tblBlogSettingCreateTime entity
  , unBlogSettingUpdateTime = tblBlogSettingUpdateTime entity
  , unBlogSettingVersion = tblBlogSettingVersion entity
  }

toMedia ::
  Entity TblMedia
  -> Media
toMedia (Entity key entity) = Media {
    unMediaId = fromTblMediaKey key
  , unMediaTitle = tblMediaTitle entity
  , unMediaDir = tblMediaDir entity
  , unMediaFileName = tblMediaFileName entity
  , unMediaHash = tblMediaHash entity
  , unMediaSize = tblMediaSize entity
  , unMediaThumbDispFlag = tblMediaThumbDispFlag entity
  , unMediaMimeTypeId = fromTblMstMimeTypeKey <$> tblMediaMimeTypeId entity
  , unMediaCreateTime = tblMediaCreateTime entity
  , unMediaUpdateTime = tblMediaUpdateTime entity
  , unMediaVersion = tblMediaVersion entity
  , unMediaAuthorId = fromTblUserKey $ tblMediaAuthorId entity
  }

toUserList ::
  (
    E.Value Int
  , E.Value (Key TblUser)
  , E.Value Text
  , E.Value Text
  , E.Value UTCTime
  , E.Value UTCTime
  , E.Value Int
  , E.Value Bool
  , E.Value Int
  , E.Value Text
  ) -> UserList
toUserList
  (
    E.Value rownum
  , E.Value key
  , E.Value name
  , E.Value email
  , E.Value ctime
  , E.Value utime
  , E.Value version
  , E.Value delflag
  , E.Value level
  , E.Value permname
  ) = UserList {
    unUserListRowNum = rownum
  , unUserListUserId = fromTblUserKey key
  , unUserListUserName = name
  , unUserListEmail = email
  , unUserListCreateTime = ctime
  , unUserListUpdateTime = utime
  , unUserListVersion = version
  , unUserListDeleteFlag = delflag
  , unUserListPermLevel = level
  , unUserListPermName = permname
  }

toUser ::
  Entity TblUser
  -> User
toUser (Entity key entity) = User {
    unUserId = fromTblUserKey key
  , unUserEmail = tblUserEmail entity
  , unUserPasswd = tblUserPassword entity
  , unUserUsername = tblUserUsername entity
  , unUserProfile =  tblUserProfile entity
  , unUserPermId = fromTblMstUserPermKey $ tblUserUserPermId entity
  , unUserCreateTime = tblUserCreateTime entity
  , unUserUpdateTime = tblUserUpdateTime entity
  , unUserVersion = tblUserVersion entity
  , unUserDeleteFlag = tblUserDeleteFlag entity
}

toPost ::
  Entity TblPost
  -> Post
toPost (Entity key entity) = Post {
    unPostId = fromTblPostKey key
  , unPostTitle = tblPostTitle entity
  , unPostContent = tblPostContent entity
  , unPostTags = tblPostTags entity
  , unPostHtml = tblPostHtml entity
  , unPostSlug = tblPostSlug entity
  , unPostUrlpath = tblPostUrlpath entity
  , unPostInputType = tblPostInputType entity
  , unPostStatus = tblPostStatus entity
  , unPostPublishDate = tblPostPublishDate entity
  , unPostDescription = tblPostDescription entity
  , unPostKeywords = tblPostKeywords entity
  , unPostRobots = tblPostRobots entity
  , unPostOgImg = tblPostOgImg entity
  , unPostOgTitle = tblPostOgTitle entity
  , unPostOgUrl = tblPostOgUrl entity
  , unPostOgSiteName = tblPostOgSiteName entity
  , unPostOgDesc = tblPostOgDesc entity
  , unPostOgPageType = tblPostOgPageType entity
  , unPostCreateTime = tblPostCreateTime entity
  , unPostUpdateTime = tblPostUpdateTime entity
  , unPostAuthorId = fromTblUserKey $ tblPostAuthorId entity
  , unPostVersion = tblPostVersion entity
}

toMstTag ::
  Entity TblMstTag
  -> MstTag
toMstTag (Entity key entity) = MstTag {
    unMstTagId = fromTblMstTagKey key
  , unMstTagName = tblMstTagName entity
  }

toMstUserPerm ::
  Entity TblMstUserPerm
  -> MstUserPerm
toMstUserPerm (Entity key entity) = MstUserPerm {
    unMstUserPermId = fromTblMstUserPermKey key
  , unMstUserPermLevel = tblMstUserPermLevel entity
  , unMstUserDispOrder = tblMstUserPermDispOrder entity
  , unMstUserName = tblMstUserPermName entity
  , unMstUserCreateTime = tblMstUserPermCreateTime entity
  , unMstUserUpdateTime = tblMstUserPermUpdateTime entity
  , unMstUserVersion = tblMstUserPermVersion entity
  }

toMediaList ::
  (
    E.Value Int
  , E.Value (Key TblMedia)
  , E.Value (Maybe Text)
  , E.Value Text
  , E.Value Text
  , E.Value Int
  , E.Value Bool
  , E.Value UTCTime
  , E.Value UTCTime
  , E.Value (Maybe Text)
  , E.Value (Maybe Text)
  , E.Value Int
  ) -> MediaList
toMediaList
  (
    E.Value rownum
  , E.Value key
  , E.Value title
  , E.Value dir
  , E.Value name
  , E.Value size
  , E.Value thflag
  , E.Value ctime
  , E.Value utime
  , E.Value ext
  , E.Value mime
  , E.Value version
  ) = MediaList {
    unMediaListRowNum = rownum
  , unMediaListId = fromTblMediaKey key
  , unMediaListTitle = title
  , unMediaListDir = dir
  , unMediaListFileName = name
  , unMediaListSize = size
  , unMediaListThumbDispFlag = thflag
  , unMediaListCreateTime = ctime
  , unMediaListUpdateTime = utime
  , unMediaListExtension = ext
  , unMediaListMimeType = mime
  , unMediaListVersion = version
  }

toTagCont ::
  (
    E.Single Int64
  , E.Single Int
  , E.Single Int64
  , E.Single Text
  , E.Single UTCTime)
  -> TagContent
toTagCont (
  E.Single tid
  , E.Single rectype
  , E.Single tagid
  , E.Single title
  , E.Single pubdate
  ) = case toEnum rectype of
        PostTag -> TagContPost $ TagInfo tid tagid title pubdate
        FreeTag -> TagContFree $ TagInfo tid tagid title pubdate

toBlogAccess ::
  (
    E.Single Day
  , E.Single Int)
  -> BlogAccess
toBlogAccess (
    E.Single date
  , E.Single tcnt
  ) = BlogAccess {
        unBlogAccessDate = date
      , unBlogAccessTatolCnt = tcnt
      }

checkVersion ::
  Int
  -> Int
  -> Bool
checkVersion recver postver
  | recver == (postver) = True
  | otherwise = False

getTblMstUserPerm ::
  Handler [MstUserPerm]
getTblMstUserPerm = runDB $ do
  list <- selectList [] [ Asc TblMstUserPermDispOrder ]
  return $ map toMstUserPerm list

eitherToMaybe :: MonadResource m =>
  Either a b
  -> m (Maybe b)
eitherToMaybe e = case e of
  Right val -> return $ Just val
  Left _ -> return Nothing

eitherToList :: MonadResource m =>
  Either a [b]
  -> m [b]
eitherToList e = case e of
  Right val@(_:_) -> return val
  Right [] -> return []
  Left _ -> return []

eitherToBool :: MonadResource m =>
  Either a Bool
  -> m Bool
eitherToBool e = case e of
  Right v -> return v
  Left _ -> return False

eitherToTouple :: MonadResource m =>
  Either a (Int, [b])
  -> m (Int, [b])
eitherToTouple e = case e of
  Right (cnt, xs) -> return (cnt, xs)
  Left _ -> return (0, mempty)

initPublishDate ::
  Int
  -> UTCTime
  -> Maybe UTCTime
initPublishDate state n =
  case toEnum state of
    Published -> Just n
    _ -> Nothing

updatePublishDate ::
  Int
  -> Maybe UTCTime
  -> UTCTime
  -> Maybe UTCTime
updatePublishDate state cur n =
  if state == fromEnum Published && isNothing cur then
    Just n
  else
    cur

convertMarkdownContents :: Text -> Int -> IO (Maybe Text)
convertMarkdownContents src contType =
  if contType == (fromEnum ContTypeMarkdown)
  then mdToHtml src
  else return Nothing
