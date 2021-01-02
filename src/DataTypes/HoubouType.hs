{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DataTypes.HoubouType (
    ErrHoubou(..)
  , PublishStatus(..)
  , PubViewStatus(..)
  , ComStatus(..)
  , TextContType(..)
  , PageInfo(..)
  , PostList(..)
  , Free(..)
  , FreeList(..)
  , Paginator(..)
  , Post(..)
  , Posted(..)
  , Frame(..)
  , FrameList(..)
  , FreeFrame(..)
  , FreeFrameList(..)
  , RecVersion(..)
  , BlogSetting(..)
  , UserList(..)
  , User(..)
  , MstUserPerm(..)
  , Media(..)
  , MediaList(..)
  , MstMimeType(..)
  , PageMeta(..)
  , Result(..)
  , MstTag(..)
  , TagContent(..)
  , TagInfo(..)
  , BlogAccess(..)
  , HResult
  , HResultErrParam
  , TagStr
  , TagList
  , PageType(..)
  , PostForm(..)
  , FreeForm(..)
  , HbUrl(..)
) where

import Data.Default
import Import.NoFoundation
import qualified Prelude as P
import Data.Maybe
import Data.Text

instance Default Text where
    def = Data.Text.empty

instance Default UTCTime where
    def = (P.read "2020-08-04 06:44:18.000000 UTC") :: UTCTime

type HResult a = Either ErrHoubou a
type HResultErrParam a b = Either (ErrHoubou, a) b

data Result = Success | Failure deriving(Show, Eq)

instance Enum Result where
    fromEnum = fromJust . flip lookup tblResult
    toEnum = fromJust . flip lookup (P.map swap tblResult)

tblResult :: [(Result, Int)]
tblResult = [(Success, 1), (Failure, 0)]

data PublishStatus =
    Published
  | UnPublished
  | Draft
  | UnkStatus
  deriving(Show, Eq)

instance Enum PublishStatus where
    fromEnum = fromJust . flip lookup tblPublishStatus
    toEnum = fromJust . flip lookup (P.map swap tblPublishStatus)

tblPublishStatus :: [(PublishStatus, Int)]
tblPublishStatus = [
    (Draft, 3)
  , (Published, 2)
  , (UnPublished, 1)
  , (UnkStatus, -1)
  ]

data PubViewStatus =
    ViewAll
  | ViewPublished
  | ViewDraft
  deriving(Show, Eq)

instance Enum PubViewStatus where
    fromEnum = fromJust . flip lookup tblPubViewStatus
    toEnum = fromJust . flip lookup (P.map swap tblPubViewStatus)

tblPubViewStatus :: [(PubViewStatus, Int)]
tblPubViewStatus = [
    (ViewAll, 1)
  , (ViewPublished, 2)
  , (ViewDraft, 3)

  ]

data ComStatus =
    Approval
  | UnApproval
  deriving(Show, Eq)

instance Enum ComStatus where
    fromEnum = fromJust . flip lookup tblComStatus
    toEnum = fromJust . flip lookup (P.map swap tblComStatus)

tblComStatus :: [(ComStatus, Int)]
tblComStatus = [(Approval, 2), (UnApproval, 1)]

data TextContType =
    ContTypeMarkdown
  | ContTypeHtml
  deriving(Eq, Show)

instance Enum TextContType where
    fromEnum = fromJust . flip lookup tblContType
    toEnum = fromJust . flip lookup (P.map swap tblContType)

tblContType :: [(TextContType, Int)]
tblContType = [(ContTypeMarkdown, 1), (ContTypeHtml, 2)]

data ErrHoubou =
    ErrRecNotFound
  | ErrParam
  | ErrRegister
  | ErrRecNotAuthor
  | ErrNotDelete
  | ErrRecVersion
  | ErrServiceException
  | ErrNotInitialized
  | ErrFileNotFound
  | ErrPostUnitialize
  | ErrFreeUnitialize
  | ErrTemplateUnInit
  | ErrTagContUnitialize
  | ErrRecNotUnique deriving(Show, Eq)

type TagList = [Text]
type TagStr = Text

-- ページ情報
data PageInfo = PageInfo {
    unPageNum :: Int
  , unPagePerLine :: Int
} deriving(Show, Eq)

-- ブログリスト
data PostList = PostList {
    unPostListRowNum :: Int
  , unPostListId :: Int64
  , unPostListStatus ::  Int
  , unPostListTitle :: Text
  , unPostListSlug :: Maybe Text
  , unPostListUrlpath :: Maybe Text
  , unPostListPublishDate :: Maybe UTCTime
  , unPostListCreateTime :: UTCTime
  , unPostListVersion :: Int
  , unPostListPostCntTotal :: Int
  , unPostListComApprovCnt :: Int
  , unPostListViewCnt :: Int
} deriving(Show, Eq)

data FreeList = FreeList {
    unFreeListRowNum :: Int
  , unFreeListId :: Int64
  , unFreeListTitle :: Text
  , unFreeListSlug :: Maybe Text
  , unFreeListUrlpath :: Maybe Text
  , unFreeListPublishDate :: Maybe UTCTime
  , unFreeListStatus :: Int
  , unFreeListAuthorId :: Int64
  , unFreeListCreateTime :: UTCTime
  , unFreeListUpdateTime :: UTCTime
  , unFreeListVersion :: Int
  , unFreeListViewCnt :: Int
} deriving(Show, Eq)

data Paginator = Paginator {
    unIsPager :: Bool
  , unCurPage :: Int
  , unPrevPage :: Int
  , unNumsPages :: [Int]
  , unNextPage :: Int
} deriving(Eq, Show)

data Post = Post {
    unPostId :: Int64
  , unPostTitle :: Text
  , unPostContent :: Text
  , unPostTags :: Maybe Text
  , unPostHtml :: Maybe Text
  , unPostSlug :: Maybe Text
  , unPostUrlpath :: Maybe Text
  , unPostInputType :: Int
  , unPostStatus :: Int
  , unPostPublishDate :: Maybe UTCTime
  , unPostDescription :: Maybe Text
  , unPostKeywords :: Maybe Text
  , unPostRobots :: Maybe Text
  , unPostOgImg :: Maybe Text
  , unPostOgTitle :: Maybe Text
  , unPostOgUrl :: Maybe Text
  , unPostOgSiteName :: Maybe Text
  , unPostOgDesc :: Maybe Text
  , unPostOgPageType :: Maybe Text
  , unPostCreateTime :: UTCTime
  , unPostUpdateTime :: UTCTime
  , unPostAuthorId :: Int64
  , unPostVersion :: Int
} deriving(Eq, Show, Default, Generic)

data Posted = Posted {
    unPostedDate :: Text
  , unPostedPosts :: [Post]
} deriving(Eq, Show)

data Frame = Frame {
    unFrameId :: Int64
  , unFrameName :: Maybe Text
  , unFrameHtml :: Maybe Text
  , unFrameCss :: Maybe Text
  , unFrameValidFlag :: Bool
  , unFramePublishDate :: Maybe UTCTime
  , unFrameCreateTime :: UTCTime
  , unFrameUpdateTime :: UTCTime
  , unFrameVersion :: Int
} deriving(Eq, Show)

data RecVersion = RecVersion {
  unRecVersion :: Int
} deriving(Eq, Show)

data Free = Free {
    unFreeId :: Int64
  , unFreeTitle :: Text
  , unFreeContent :: Text
  , unFreeHtml :: Maybe Text
  , unFreeCss :: Maybe Text
  , unFreeSlug :: Maybe Text
  , unFreeUrlpath :: Maybe Text
  , unFreeInputType :: Int
  , unFreeStatus :: Int
  , unFreeTags :: Maybe Text
  , unFreePublishDate :: Maybe UTCTime
  , unFreeDescription :: Maybe Text
  , unFreeKeywords :: Maybe Text
  , unFreeRobots :: Maybe Text
  , unFreeOgImg :: Maybe Text
  , unFreeOgTitle :: Maybe Text
  , unFreeOgUrl :: Maybe Text
  , unFreeOgSiteName :: Maybe Text
  , unFreeOgDesc :: Maybe Text
  , unFreeOgPageType :: Maybe Text
  , unFreeCreateTime :: UTCTime
  , unFreeUpdateTime :: UTCTime
  , unFreeAuthorId :: Int64
  , unFreeVersion :: Int
} deriving(Eq, Show, Default, Generic)

data FrameList = FrameList {
    unFrameListRowNum :: Int
  , unFrameListId :: Int64
  , unFrameListName :: Maybe Text
  , unFrameListValidFlag :: Bool
  , unFrameListPublishDate :: Maybe UTCTime
  , unFrameListCreateTime :: UTCTime
  , unFrameListUpdateTime :: UTCTime
  , unFrameListVersion :: Int
} deriving(Eq, Show)

data FreeFrameList = FreeFrameList {
    unFreeFrameListRowNum :: Int
  , unFreeFrameListId :: Int64
  , unFreeFrameListName :: Maybe Text
  , unFreeFrameListValidFlag :: Bool
  , unFreeFrameListPublishDate :: Maybe UTCTime
  , unFreeFrameListCreateTime :: UTCTime
  , unFreeFrameListUpdateTime :: UTCTime
  , unFreeFrameListVersion :: Int
} deriving(Eq, Show)

data FreeFrame = FreeFrame {
    unFreeFrameId :: Int64
  , unFreeFrameName :: Maybe Text
  , unFreeFrameHtml :: Maybe Text
  , unFreeFrameCss :: Maybe Text
  , unFreeFrameValidFlag :: Bool
  , unFreeFramePublishDate :: Maybe UTCTime
  , unFreeFrameCreateTime :: UTCTime
  , unFreeFrameUpdateTime :: UTCTime
  , unFreeFrameVersion :: Int
} deriving(Eq, Show)

data BlogSetting = BlogSetting {
    unBlogSettingId :: Int64
  , unBlogSettingIdent :: Text
  , unBlogSettingBlogName :: Text
  , unBlogSettingBlogUrl :: Text
  , unBlogSettingPostNum :: Int
  , unBlogSettingMediaUrl :: Text
  , unBlogSettingMediaDir :: Text
  , unBlogSettingUploadSize :: Int
  , unBlogSettingSessionTimeout :: Int
  , unBlogSettingAdstxt :: Maybe Text
  , unBlogSettingCreateTime :: UTCTime
  , unBlogSettingUpdateTime :: UTCTime
  , unBlogSettingVersion :: Int
} deriving(Eq, Show)

data UserList = UserList {
    unUserListRowNum :: Int
  , unUserListUserId :: Int64
  , unUserListUserName :: Text
  , unUserListEmail :: Text
  , unUserListCreateTime :: UTCTime
  , unUserListUpdateTime :: UTCTime
  , unUserListVersion :: Int
  , unUserListDeleteFlag :: Bool
  , unUserListPermLevel :: Int
  , unUserListPermName :: Text
} deriving(Eq, Show)

data User = User {
    unUserId :: Int64
  , unUserEmail :: Text
  , unUserPasswd :: Maybe Text
  , unUserUsername :: Text
  , unUserProfile :: Text
  , unUserPermId :: Int64
  , unUserCreateTime :: UTCTime
  , unUserUpdateTime :: UTCTime
  , unUserVersion :: Int
  , unUserDeleteFlag :: Bool
} deriving(Eq, Show)

data MstUserPerm = MstUserPerm {
    unMstUserPermId :: Int64
  , unMstUserPermLevel :: Int
  , unMstUserDispOrder :: Int
  , unMstUserName :: Text
  , unMstUserCreateTime :: UTCTime
  , unMstUserUpdateTime :: UTCTime
  , unMstUserVersion :: Int
} deriving(Eq, Show)

data Media = Media {
    unMediaId :: Int64
  , unMediaTitle :: Maybe Text
  , unMediaDir :: Text
  , unMediaFileName :: Text
  , unMediaHash :: Maybe Text
  , unMediaSize :: Int
  , unMediaThumbDispFlag :: Bool
  , unMediaMimeTypeId :: Maybe Int64
  , unMediaCreateTime :: UTCTime
  , unMediaUpdateTime :: UTCTime
  , unMediaVersion :: Int
  , unMediaAuthorId :: Int64
  } deriving(Eq, Show)

data MediaList = MediaList {
    unMediaListRowNum :: Int
  , unMediaListId :: Int64
  , unMediaListTitle :: Maybe Text
  , unMediaListDir :: Text
  , unMediaListFileName :: Text
  , unMediaListSize :: Int
  , unMediaListThumbDispFlag :: Bool
  , unMediaListCreateTime :: UTCTime
  , unMediaListUpdateTime :: UTCTime
  , unMediaListExtension :: Maybe Text
  , unMediaListMimeType :: Maybe Text
  , unMediaListVersion :: Int
} deriving(Eq, Show)

data MstMimeType = MstMimeType {
    unMstMimeTypeId :: Int64
  , unMstMimeTypeExtension :: Text
  , unMstMimeTypeMimeType :: Text
  , unMstMimeTypeCreateTime :: UTCTime
  , unMstMimeTypeUpdateTime :: UTCTime
  , unMstMimeTypeVersion :: Int
  , unMstMimeTypeDeleteFlag :: Bool
  } deriving(Eq, Show)

data PageMeta = PageMeta {
    unPageMetaBlogName :: Text
  , unPageMetaTitle :: Text
  , unPageMetaBlogUrl :: Text
  , unPageMetaFrameCss :: Text
  , unPageMetaMediaUrl :: Text
  , unPageMetaCanonicalUrl :: Text
  , unPageMetaDescription :: Text
  , unPageMetaKeywords :: Text
  , unPageMetaRobots :: Text
  , unPageMetaOgImg :: Text
  , unPageMetaOgTitle :: Text
  , unPageMetaOgUrl :: Text
  , unPageMetaOgSiteName :: Text
  , unPageMetaOgDesc :: Text
  , unPageMetaOgPageType :: Text
  } deriving(Eq, Show)

data MstTag = MstTag {
    unMstTagId :: Int64
  , unMstTagName :: Text
  } deriving(Eq, Show)

data TagContent =
    TagContPost TagInfo
  | TagContFree TagInfo
  deriving (Eq, Show)

data TagInfo = TagInfo {
    unTagInfoId :: Int64
  , unTagInfoTagId :: Int64
  , unTagInfoTitle :: Text
  , unTagInfoPosted :: UTCTime
  } deriving (Eq, Show)

data PageType = TypePost | TypeFree deriving(Show, Eq)

instance Enum PageType where
    fromEnum = fromJust . flip lookup tblPageType
    toEnum = fromJust . flip lookup (P.map swap tblPageType)

tblPageType :: [(PageType, Int)]
tblPageType = [(TypePost, 1), (TypeFree, 2)]

data BlogAccess = BlogAccess {
    unBlogAccessDate :: Day
  , unBlogAccessTatolCnt :: Int
  } deriving(Show, Eq)

data PostForm = PostForm {
    unPostFormId :: Int64
  , unPostFormTitle :: Text
  , unPostFormContent :: Text
  , unPostFormSlug :: Maybe Text
  , unPostFormUrlpath :: Maybe Text
  , unPostFormInputType :: Int
  , unPostFormStatus :: Int
  , unPostFormTags :: Maybe Text
  , unPostFormDescription :: Maybe Text
  , unPostFormKeywords :: Maybe Text
  , unPostFormRobots :: Maybe Text
  , unPostFormOgImg :: Maybe Text
  , unPostFormOgTitle :: Maybe Text
  , unPostFormOgUrl :: Maybe Text
  , unPostFormOgSiteName :: Maybe Text
  , unPostFormOgDesc :: Maybe Text
  , unPostFormOgPageType :: Maybe Text
  , unPostFormVersion :: Int
}

data FreeForm = FreeForm {
    unFreeFormId :: Int64
  , unFreeFormTitle :: Text
  , unFreeFormContent :: Text
  , unFreeFormSlug :: Maybe Text
  , unFreeFormUrlpath :: Maybe Text
  , unFreeFormCss :: Maybe Text
  , unFreeFormInputType :: Int
  , unFreeFormStatus :: Int
  , unFreeFormTags :: Maybe Text
  , unFreeFormDescription :: Maybe Text
  , unFreeFormKeywords :: Maybe Text
  , unFreeFormRobots :: Maybe Text
  , unFreeFormOgImg :: Maybe Text
  , unFreeFormOgTitle :: Maybe Text
  , unFreeFormOgUrl :: Maybe Text
  , unFreeFormOgSiteName :: Maybe Text
  , unFreeFormOgDesc :: Maybe Text
  , unFreeFormOgPageType :: Maybe Text
  , unFreeFormVersion :: Int
}

data HbUrl = HbUrl {
    unHbUrlId :: Int64
  , unHbUrlType :: PageType
  , unHbUrlSlug :: Maybe Text
  , unHbUrlUrlpath :: Maybe Text
  , unHbUrlUpdateTime :: UTCTime
  , unHbUrlAuthorId :: Int64
} deriving (Show)
