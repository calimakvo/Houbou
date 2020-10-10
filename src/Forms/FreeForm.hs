{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.FreeForm (
    freeForm
  , FreeForm(..)
  ) where

import Import
import DataTypes.HoubouType
import Forms.FormValid
import Forms.CommonForm
import UrlParam.Page
import Libs.Common

data FreeForm = FreeForm {
    unFreeFormId :: Int64
  , unFreeFormTitle :: Text
  , unFreeFormContent :: Text
  , unFreeFormSlug :: Maybe Text
  , unFreeFormUrlpath :: Maybe Text
  , unFreeFormCss :: Maybe Text
  , unFreeFormInputType :: Int
  , unFreeFormTags :: Maybe Text
  , unFreeFormVersion :: Int
} deriving(Eq, Show)

freeForm :: Maybe Free -> Html -> MForm Handler (FormResult FreeForm, Widget)
freeForm free extra = do
  master <- getYesod
  t <- liftIO getTm
  let titleLen = appFreeTitleMaxLength $ appSettings master
      contLen = appFreeContentMaxLength $ appSettings master
      cssLen = appFreeCssMaxLength $ appSettings master
      slugLen = appSlugTextMaxLength $ appSettings master
      tagLen = appTagMaxLength $ appSettings master
      tagOneLen = appTagOneMaxLength $ appSettings master
      freeId = fromMaybe 0 (unFreeId <$> free)
      version = fromMaybe 0 (unFreeVersion <$> free)
      selInpTyp = fromMaybe 1 (unFreeInputType <$> free)
      urlpath = if freeId > 0 then join (unFreeUrlpath <$> free) else (Just $ toUrlPath t)
  inpTyp <- liftHandler getInputTypeTouple
  (freeIdRes, freeIdView) <- mreq hiddenField freeIdFieldSet (Just freeId)
  (titleRes, titleView) <- mreq (freeTitleField titleLen) freeTitleFieldSet (unFreeTitle <$> free)
  (contRes, contView) <- mreq (freeContField contLen) freeContFieldSet (Textarea <$> (unFreeContent <$> free))
  (cssRes, cssView) <- mopt (freeCssField cssLen) freeCssFieldSet ((Textarea <$>) <$> (unFreeCss <$> free))
  (slugRes, slugView) <- mopt (slugFreeField slugLen freeIdRes urlpath) slugFreeFieldSet (unFreeSlug <$> free)  
  (inpTypRes, _) <- mreq (radioFieldList inpTyp) inputTypeRadioFieldSet (unFreeInputType <$> free)
  (tagRes, tagView) <- mopt (tagField tagLen tagOneLen) tagFieldSet (unFreeTags <$> free)
  (verRes, verView) <- mreq hiddenIdField versionFieldSet (Just version)
  let formParam = FreeForm
                    <$> freeIdRes
                    <*> titleRes
                    <*> (unTextarea <$> contRes)
                    <*> slugRes
                    <*> initFormUrlpath slugRes urlpath
                    <*> (liftM unTextarea <$> cssRes)
                    <*> inpTypRes
                    <*> tagRes
                    <*> verRes
      widget = $(whamletFile "templates/freepage_form.hamlet")
  return (formParam, widget)
