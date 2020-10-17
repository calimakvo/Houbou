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
  , unFreeFormStatus :: Int
  , unFreeFormTags :: Maybe Text
  , unFreeFormDescription :: Maybe Text
  , unFreeFormKeywords :: Maybe Text
  , unFreeFormRobots :: Maybe Text
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
      dscLen = appMetaDescriptionMaxLength $ appSettings master
      kwdLen = appMetaKeywordsMaxLength $ appSettings master
      robLen = appMetaRobotsMaxLength $ appSettings master
      freeId = fromMaybe 0 (unFreeId <$> free)
      robots = fromMaybe (Just "index,follow") (unFreeRobots <$> free)
      version = fromMaybe 0 (unFreeVersion <$> free)
      selInpTyp = fromMaybe 1 (unFreeInputType <$> free)
      selStsTyp = fromMaybe (fromEnum UnPublished) (unFreeStatus <$> free)
      urlpath = if freeId > 0 then join (unFreeUrlpath <$> free) else (Just $ toUrlPath t)
  inpTyp <- liftHandler getInputTypeTouple
  stsTyp <- liftHandler getStatusSelectTouple
  (freeIdRes, freeIdView) <- mreq hiddenField freeIdFieldSet (Just freeId)
  (titleRes, titleView) <- mreq (freeTitleField titleLen) freeTitleFieldSet (unFreeTitle <$> free)
  (contRes, contView) <- mreq (freeContField contLen) freeContFieldSet (Textarea <$> (unFreeContent <$> free))
  (cssRes, cssView) <- mopt (freeCssField cssLen) freeCssFieldSet ((Textarea <$>) <$> (unFreeCss <$> free))
  (slugRes, slugView) <- mopt (slugFreeField slugLen freeIdRes urlpath) slugFreeFieldSet (unFreeSlug <$> free)  
  (inpTypRes, _) <- mreq (radioFieldList inpTyp) inputTypeRadioFieldSet (unFreeInputType <$> free)
  (stsTypRes, stsTypView) <- mreq (radioFieldList stsTyp) statusTypeRadioFieldSet (unFreeStatus <$> free)
  (tagRes, tagView) <- mopt (tagField tagLen tagOneLen) tagFieldSet (unFreeTags <$> free)
  (dscRes, dscView) <- mopt (descriptionField dscLen)
                           descriptionFieldSet (Just (Textarea <$> (join (unFreeDescription <$> free))))
  (kwdRes, kwdView) <- mopt (keywordsField kwdLen) keywordsFieldSet (unFreeKeywords <$> free)
  (robRes, robView) <- mopt (robotsField robLen) robotsFieldSet(Just robots)
  (verRes, verView) <- mreq hiddenIdField versionFieldSet (Just version)
  let formParam = FreeForm
                  <$> freeIdRes
                  <*> titleRes
                  <*> (unTextarea <$> contRes)
                  <*> slugRes
                  <*> initFormUrlpath slugRes urlpath
                  <*> (liftM unTextarea <$> cssRes)
                  <*> inpTypRes
                  <*> stsTypRes
                  <*> tagRes
                  <*> ((fmap . fmap) unTextarea  dscRes)
                  <*> kwdRes
                  <*> robRes
                  <*> verRes
      widget = $(whamletFile "templates/freepage_form.hamlet")
  return (formParam, widget)
