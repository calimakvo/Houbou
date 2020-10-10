{-# LANGUAGE OverloadedStrings #-}

module Libs.Mapper (
    postFormToPost
  , freeFormToFree
  , frameFormToFrame
  , freeFrameFormToFreeFrame
  , blogSettingFormToBlogSetting
  , userFormToForm
  , userNewFormToForm
  , mediaFormToMedia
  ) where

import Data.Text
import DataTypes.HoubouType
import Libs.Common
import Forms.PostForm
import Forms.FreeForm
import Forms.FrameForm
import Forms.FreeFrameForm
import Forms.BlogSettingForm
import Forms.UserForm
import Forms.UserNewForm
import Forms.MediaMdfForm

postFormToPost ::
  PostForm
  -> Post
postFormToPost form = Post {
    unPostId = unPostFormId form
  , unPostTitle = unPostFormTitle form
  , unPostContent = unPostFormContent form
  , unPostSlug = unPostFormSlug form
  , unPostUrlpath = unPostFormUrlpath form
  , unPostTags = unPostFormTags form
  , unPostHtml = Nothing
  , unPostInputType = unPostFormInputType form
  , unPostStatus = fromEnum UnPublished
  , unPostPublishDate = Nothing
  , unPostCreateTime = dummyUtc
  , unPostUpdateTime = dummyUtc
  , unPostAuthorId = 0
  , unPostVersion = unPostFormVersion form
  }

freeFormToFree ::
  FreeForm
  -> Free
freeFormToFree form = Free {
    unFreeId = unFreeFormId form
  , unFreeTitle = unFreeFormTitle form
  , unFreeContent = unFreeFormContent form
  , unFreeHtml = Nothing
  , unFreeCss = unFreeFormCss form
  , unFreeSlug = unFreeFormSlug form
  , unFreeUrlpath = unFreeFormUrlpath form
  , unFreeInputType = unFreeFormInputType form
  , unFreeTags = unFreeFormTags form
  , unFreeStatus = fromEnum UnPublished
  , unFreePublishDate = Nothing
  , unFreeCreateTime = dummyUtc
  , unFreeUpdateTime = dummyUtc
  , unFreeAuthorId = 0
  , unFreeVersion = unFreeFormVersion form
  }

frameFormToFrame ::
  FrameForm
  -> Frame
frameFormToFrame form = Frame {
    unFrameId = unFrameFormId form
  , unFrameName = unFrameFormName form
  , unFrameHtml = unFrameFormHtml form
  , unFrameCss = unFrameFormCss form
  , unFrameValidFlag = False
  , unFramePublishDate = Nothing
  , unFrameCreateTime = dummyUtc
  , unFrameUpdateTime = dummyUtc
  , unFrameVersion = unFrameFormVersion form
  }  

freeFrameFormToFreeFrame ::
  FreeFrameForm
  -> FreeFrame
freeFrameFormToFreeFrame form = FreeFrame {
    unFreeFrameId = unFreeFrameFormId form
  , unFreeFrameName = unFreeFrameFormName form
  , unFreeFrameHtml = unFreeFrameFormHtml form
  , unFreeFrameCss = unFreeFrameFormCss form
  , unFreeFrameValidFlag = False
  , unFreeFramePublishDate = Nothing
  , unFreeFrameCreateTime = dummyUtc
  , unFreeFrameUpdateTime = dummyUtc
  , unFreeFrameVersion = unFreeFrameFormVersion form
  }

blogSettingFormToBlogSetting ::
  BlogSettingForm
  -> BlogSetting
blogSettingFormToBlogSetting form = BlogSetting {
    unBlogSettingId = 0
  , unBlogSettingIdent = "NOTINIT"
  , unBlogSettingBlogName = unBlogSettingFormBlogName form
  , unBlogSettingBlogUrl = unBlogSettingFormBlogUrl form
  , unBlogSettingPostNum = unBlogSettingFormPostNum form
  , unBlogSettingMediaUrl = unBlogSettingFormMediaUrl form
  , unBlogSettingMediaDir = unBlogSettingFormMediaDir form
  , unBlogSettingUploadSize = unBlogSettingFormUploadSize form
  , unBlogSettingAdstxt = unBlogSettingFormAdstxt form
  , unBlogSettingCreateTime = dummyUtc
  , unBlogSettingUpdateTime = dummyUtc
  , unBlogSettingVersion = unBlogSettingFormVersion form
  }

userFormToForm ::
  UserForm
  -> User
userFormToForm form = User {
    unUserId = unUserFormId form
  , unUserEmail = unUserFormEmail form
  , unUserPasswd = unUserFormPasswd form
  , unUserUsername = unUserFormUsername form
  , unUserProfile = unUserFormProfile form
  , unUserPermId = unUserFormUserPermId form
  , unUserCreateTime = dummyUtc
  , unUserUpdateTime = dummyUtc
  , unUserVersion = unUserFormVersion form
  , unUserDeleteFlag = False
  }

userNewFormToForm ::
  UserNewForm
  -> User
userNewFormToForm form = User {
    unUserId = unUserNewFormId form
  , unUserEmail = unUserNewFormEmail form
  , unUserPasswd = Just $ unUserNewFormPasswd form
  , unUserUsername = unUserNewFormUsername form
  , unUserProfile = unUserNewFormProfile form
  , unUserPermId = unUserNewFormUserPermId form
  , unUserCreateTime = dummyUtc
  , unUserUpdateTime = dummyUtc
  , unUserVersion = unUserNewFormVersion form
  , unUserDeleteFlag = False
  }

mediaFormToMedia ::
  MediaMdfForm
  -> Media
mediaFormToMedia form = Media {
    unMediaId = unMediaMdfFormId form
  , unMediaTitle = unMediaMdfFormTitle form
  , unMediaDir = empty
  , unMediaFileName = empty
  , unMediaHash = Nothing
  , unMediaSize = 0
  , unMediaThumbDispFlag = False
  , unMediaMimeTypeId = Just 0
  , unMediaCreateTime = dummyUtc
  , unMediaUpdateTime = dummyUtc
  , unMediaVersion = unMediaMdfFormVersion form
  , unMediaAuthorId = 0
  }
