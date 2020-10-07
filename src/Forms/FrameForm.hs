{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.FrameForm (
    frameForm
  , FrameForm(..)
  ) where

import Import
import DataTypes.HoubouType
import Forms.FormValid
import Forms.CommonForm
import UrlParam.Page

data FrameForm = FrameForm {
    unFrameFormId :: Int64
  , unFrameFormName :: Maybe Text
  , unFrameFormHtml :: Maybe Text
  , unFrameFormCss :: Maybe Text
  , unFrameFormVersion :: Int
} deriving(Eq, Show)

frameForm :: Maybe Frame -> Html -> MForm Handler (FormResult FrameForm, Widget)
frameForm frame extra = do
  master <- getYesod
  let htmlLen = appFrameHtmlMaxLength $ appSettings master
      cssLen = appFrameCssMaxLength $ appSettings master
      nameLen = appFrameNameMaxLength $ appSettings master
      frameId = fromMaybe 0 (unFrameId <$> frame)
      version = fromMaybe 0 (unFrameVersion <$> frame)
  (frameIdRes, frameIdView) <- mreq hiddenField frameIdFieldSet (Just frameId)
  (frameNameRes, frameNameView) <- mopt (frameNameField nameLen) frameNameFieldSet (unFrameName <$> frame)
  (frameHtmlRes, frameHtmlView) <- mopt (frameHtmlField htmlLen) frameHtmlFieldSet ((Textarea <$>) <$> (unFrameHtml <$> frame))
  (frameCssRes, frameCssView) <- mopt (frameCssField cssLen) frameCssFieldSet ((Textarea <$>) <$> (unFrameCss <$> frame))
  (frameVerRes, frameVerView) <- mreq hiddenIdField versionFieldSet (Just version)
  let formParam = FrameForm
                    <$> frameIdRes
                    <*> frameNameRes
                    <*> (liftM unTextarea <$> frameHtmlRes)
                    <*> (liftM unTextarea <$> frameCssRes)
                    <*> frameVerRes
      widget = $(whamletFile "templates/frame_form.hamlet")
  return (formParam, widget)
