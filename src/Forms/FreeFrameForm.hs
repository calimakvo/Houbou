{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.FreeFrameForm (
    freeFrameForm
  , FreeFrameForm(..)
  ) where

import Import
import DataTypes.HoubouType
import Forms.FormValid
import Forms.CommonForm
import UrlParam.Page

data FreeFrameForm = FreeFrameForm {
    unFreeFrameFormId :: Int64
  , unFreeFrameFormName :: Maybe Text
  , unFreeFrameFormHtml :: Maybe Text
  , unFreeFrameFormCss :: Maybe Text
  , unFreeFrameFormVersion :: Int
} deriving(Eq, Show)

freeFrameForm :: Maybe FreeFrame -> Html -> MForm Handler (FormResult FreeFrameForm, Widget)
freeFrameForm frame extra = do
  master <- getYesod
  let htmlLen = appFrameHtmlMaxLength $ appSettings master
      cssLen = appFrameCssMaxLength $ appSettings master
      nameLen = appFrameNameMaxLength $ appSettings master
      version = fromMaybe 0 (unFreeFrameVersion <$> frame)
      freeFrameId = fromMaybe 0 (unFreeFrameId <$> frame)
  (frameIdRes, frameIdView) <- mreq hiddenField frameIdFieldSet (Just freeFrameId)
  (frameNameRes, frameNameView) <- mopt (frameNameField nameLen) frameNameFieldSet (unFreeFrameName <$> frame)
  (frameHtmlRes, frameHtmlView) <- mopt (frameHtmlField htmlLen) frameHtmlFieldSet ((Textarea <$>) <$> (unFreeFrameHtml <$> frame))
  (frameCssRes, frameCssView) <- mopt (frameCssField cssLen) frameCssFieldSet ((Textarea <$>) <$> (unFreeFrameCss <$> frame))
  (frameVerRes, frameVerView) <- mreq hiddenIdField versionFieldSet (Just version)
  let formParam = FreeFrameForm
                    <$> frameIdRes
                    <*> frameNameRes
                    <*> (liftM unTextarea <$> frameHtmlRes)
                    <*> (liftM unTextarea <$> frameCssRes)
                    <*> frameVerRes
      widget = $(whamletFile "templates/freeframe_form.hamlet")
  return (formParam, widget)
