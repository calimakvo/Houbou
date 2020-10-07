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

data FreeForm = FreeForm {
    unFreeFormId :: Int64
  , unFreeFormTitle :: Text
  , unFreeFormContent :: Text
  , unFreeFormCss :: Maybe Text
  , unFreeFormInputType :: Int
  , unFreeFormTags :: Maybe Text
  , unFreeFormVersion :: Int
} deriving(Eq, Show)

freeForm :: Maybe Free -> Html -> MForm Handler (FormResult FreeForm, Widget)
freeForm free extra = do
  master <- getYesod
  let contLen = appFreeContentMaxLength $ appSettings master
      cssLen = appFreeCssMaxLength $ appSettings master
      nameLen = appFreeNameMaxLength $ appSettings master
      tagLen = appTagMaxLength $ appSettings master
      tagOneLen = appTagOneMaxLength $ appSettings master
      freeId = fromMaybe 0 (unFreeId <$> free)
      version = fromMaybe 0 (unFreeVersion <$> free)
      selInpTyp = fromMaybe 1 (unFreeInputType <$> free)
  inpTyp <- liftHandler getInputTypeTouple
  (idRes, idView) <- mreq hiddenField freeIdFieldSet (Just freeId)
  (titleRes, titleView) <- mreq (freeTitleField nameLen) freeTitleFieldSet (unFreeTitle <$> free)
  (contRes, contView) <- mreq (freeContField contLen) freeContFieldSet (Textarea <$> (unFreeContent <$> free))
  (cssRes, cssView) <- mopt (freeCssField cssLen) freeCssFieldSet ((Textarea <$>) <$> (unFreeCss <$> free))
  (inpTypRes, _) <- mreq (radioFieldList inpTyp) inputTypeRadioFieldSet (unFreeInputType <$> free)
  (tagRes, tagView) <- mopt (tagField tagLen tagOneLen) tagFieldSet (unFreeTags <$> free)
  (verRes, verView) <- mreq hiddenIdField versionFieldSet (Just version)
  let formParam = FreeForm
                    <$> idRes
                    <*> titleRes
                    <*> (unTextarea <$> contRes)
                    <*> (liftM unTextarea <$> cssRes)
                    <*> inpTypRes
                    <*> tagRes
                    <*> verRes
      widget = $(whamletFile "templates/freepage_form.hamlet")
  return (formParam, widget)
