{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.MediaMdfForm (
    mediaMdfForm
  , MediaMdfForm(..)
  ) where

import Import
import Data.Maybe
import UrlParam.Page
import Forms.FormValid
import Forms.CommonForm
import Libs.Common
import DataTypes.HoubouType

data MediaMdfForm = MediaMdfForm {
    unMediaMdfFormId :: Int64
  , unMediaMdfFormTitle :: Maybe Text
  , unMediaMdfFormVersion :: Int
} deriving(Eq, Show)

mediaMdfForm ::
  BlogSetting
  -> (Maybe Media)
  -> Html
  -> MForm Handler (FormResult MediaMdfForm, Widget)
mediaMdfForm setting media extra = do
  master <- getYesod
  let titleLen = appMediaTitleMaxLength $ appSettings master
      mediaId = fromMaybe 0 (unMediaId <$> media)
      version = fromMaybe 0 (unMediaVersion <$> media)
      m = fromJust media
  (mediaIdRes, mediaIdView) <- mreq hiddenField mediaIdFieldSet (Just mediaId)
  (titleRes, titleView) <- mopt (mediaTitleField titleLen) mediaTitleFieldSet (unMediaTitle <$> media)
  (verRes, verView) <- mreq hiddenIdField versionFieldSet (Just version)
  let formParam = MediaMdfForm
                    <$> mediaIdRes
                    <*> titleRes
                    <*> verRes
      widget = $(whamletFile "templates/mediamdf_form.hamlet")
  return (formParam, widget)
