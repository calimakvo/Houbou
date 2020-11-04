{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.BlogSettingForm (
    blogSettingForm
  , BlogSettingForm(..)
  ) where

import Import
import DataTypes.HoubouType
import Forms.FormValid
import Forms.CommonForm
import Libs.Common

data BlogSettingForm = BlogSettingForm {
    unBlogSettingFormBlogName :: Text
  , unBlogSettingFormBlogUrl :: Text
  , unBlogSettingFormPostNum :: Int
  , unBlogSettingFormMediaUrl :: Text
  , unBlogSettingFormMediaDir :: Text
  , unBlogSettingFormUploadSize :: Int
  , unBlogSettingFormSessionTimeout :: Int
  , unBlogSettingFormAdstxt :: Maybe Text
  , unBlogSettingFormVersion :: Int
} deriving(Eq, Show)

blogSettingForm ::
  (Maybe BlogSetting)
  -> Html
  -> MForm Handler (FormResult BlogSettingForm, Widget)
blogSettingForm setting extra = do
  master <- getYesod
  let blogNameLen = appBlogSetBlogNameMaxLength $ appSettings master
      blogUrlLen = appBlogSetBlogUrlMaxLength $ appSettings master
      blogMdaUrlLen = appBlogSetMediaUrlMaxLength $ appSettings master
      blogDirLen = appBlogSetMediaDirMaxLength $ appSettings master
      postNumMax = appBlogSetPostNumMax $ appSettings master
      uploadSizeMax = appBlogSetUploadSizeMax $ appSettings master
      sessTimeoutMin = appBlogSetSessionTimeoutMin $ appSettings master
      sessTimeoutMax = appBlogSetSessionTimeoutMax $ appSettings master
      adstxtLen = appBlogSetAdsTxtMaxLength $ appSettings master
  selector <- liftHandler $ postNumSelectTouple postNumMax
  (nameRes, nameView) <- mreq (blogNameField blogNameLen) blogNameFieldSet (unBlogSettingBlogName <$> setting)
  (urlRes, urlView) <- mreq (blogUrlField blogUrlLen) blogUrlFieldSet (unBlogSettingBlogUrl <$> setting)
  (mdaRes, mdaView) <- mreq (blogMediaUrlField blogMdaUrlLen) blogMdaUrlFieldSet (unBlogSettingMediaUrl <$> setting)
  (dirRes, dirView) <- mreq (blogMediaDirField blogDirLen) blogMdaDirFieldSet (unBlogSettingMediaDir <$> setting)
  (sizeRes, sizeView) <- mreq (uploadSizeField uploadSizeMax) uploadSizeFieldSet (unBlogSettingUploadSize <$> setting)
  (numRes, numView) <- mreq (selectFieldList selector) lineSelectFieldSet (unBlogSettingPostNum <$> setting)
  (sessRes, sessView) <- mreq (sessTimeoutField sessTimeoutMin sessTimeoutMax)
                           sessTimeoutFieldSet (unBlogSettingSessionTimeout <$> setting)
  (adsRes, adsView) <- mopt (blogAdsField adstxtLen) blogAdsFieldSet (unBlogSettingAdstxt <$> setting)
  (verRes, verView) <- mreq hiddenIdField versionFieldSet (unBlogSettingVersion <$> setting)
  let formParam = BlogSettingForm
                    <$> nameRes
                    <*> urlRes
                    <*> numRes
                    <*> mdaRes
                    <*> dirRes
                    <*> sizeRes
                    <*> sessRes
                    <*> adsRes
                    <*> verRes
      widget = $(whamletFile "templates/setting_form.hamlet")
  return (formParam, widget)
