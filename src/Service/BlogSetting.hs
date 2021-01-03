{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.BlogSetting (
    getBlogSetting
  , updateBlogSetting
  ) where

import Import
import DataTypes.HoubouType
import Libs.Common
import Service.Common

getBlogSetting ::
  Text
  -> Handler BlogSetting
getBlogSetting setId = do
  result <- getBlogSetting' setId
  case result of
    Right setting -> return setting
    Left err -> do
      $(logError) $ "getBlogSetting: Blog setting info not invalid" <> toText err
      error "Please initialize this application."

getBlogSetting' ::
  Text
  -> Handler (HResult BlogSetting)
getBlogSetting' setId = runDB $ do
  result <- getBy $ UniIdent setId
  res <- case result of
    Nothing -> return $ Left ErrNotInitialized
    Just setting -> return $ Right (toBlogSetting setting)
  return res

updateBlogSetting ::
  Text
  -> BlogSetting
  -> Handler (HResult Int64)
updateBlogSetting uni setting = runDB $ do
  let uniKey = UniIdent uni
  curRec <- getBy uniKey
  res <- case curRec of
    Nothing -> return $ Left ErrRecNotFound
    Just (Entity key record) -> do
      let pver = unBlogSettingVersion setting
          dver = tblBlogSettingVersion record
      case checkVersion dver pver of
        False -> return $ Left ErrRecVersion
        True -> do
          now <- liftIO getTm
          update key [
              TblBlogSettingBlogName =. unBlogSettingBlogName setting
            , TblBlogSettingBlogUrl =. unBlogSettingBlogUrl setting
            , TblBlogSettingPostNum =. unBlogSettingPostNum setting
            , TblBlogSettingMediaUrl =. unBlogSettingMediaUrl setting
            , TblBlogSettingMediaDir =. unBlogSettingMediaDir setting
            , TblBlogSettingUploadSize =. unBlogSettingUploadSize setting
            , TblBlogSettingSessionTimeout =. unBlogSettingSessionTimeout setting
            , TblBlogSettingAdstxt =. unBlogSettingAdstxt setting
            , TblBlogSettingBlogAuthor =. unBlogSettingBlogAuthor setting
            , TblBlogSettingBlogDesc =. unBlogSettingBlogDesc setting
            , TblBlogSettingUpdateTime =. now
            , TblBlogSettingVersion +=. 1
            ]
          return $ Right $ fromTblBlogSettingKey key
  return res
