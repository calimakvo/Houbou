{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.FreeForm (
    freeForm
  ) where

import Import
import qualified Data.Map.Ordered as O
import Forms.FormValid
import Forms.CommonForm
import DataTypes.HoubouType
import Libs.Common

freeForm ::
  Maybe Free
  -> O.OMap Int [Cate]
  -> Html
  -> MForm Handler (FormResult FreeForm, Widget)
freeForm free cate extra = do
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
      ogImgLen = appMetaOgImageMaxLength $ appSettings master
      ogTtlLen = appMetaOgTitleMaxLength $ appSettings master
      ogUrlLen = appMetaOgUrlMaxLength $ appSettings master
      ogSnmLen = appMetaOgSiteNameMaxLength $ appSettings master
      ogDscLen = appMetaOgDescMaxLength $ appSettings master
      ogPgtLen = appMetaOgPageTypeMaxLength $ appSettings master
      freeId = fromMaybe 0 (unFreeId <$> free)
      robots = fromMaybe (Just "index,follow") (unFreeRobots <$> free)
      ogpgt = fromMaybe (Just "article") (unFreeOgPageType <$> free)
      version = fromMaybe 0 (unFreeVersion <$> free)
      selInpTyp = fromMaybe 1 (unFreeInputType <$> free)
      selStsTyp = fromMaybe (fromEnum UnPublished) (unFreeStatus <$> free)
      urlpath = if freeId > 0 then join (unFreeUrlpath <$> free) else (Just $ toUrlPath t)
  inpTyp <- liftHandler getInputTypeTouple
  stsTyp <- liftHandler getStatusSelectTouple
  cateLst <- liftHandler $ cateMapToTouble cate
  (freeIdRes, freeIdView) <- mreq hiddenField freeIdFieldSet (Just freeId)
  (titleRes, titleView) <- mreq (freeTitleField titleLen) freeTitleFieldSet (unFreeTitle <$> free)
  (contRes, contView) <- mreq (freeContField contLen) freeContFieldSet (Textarea <$> (unFreeContent <$> free))
  (cssRes, cssView) <- mopt (freeCssField cssLen) freeCssFieldSet ((Textarea <$>) <$> (unFreeCss <$> free))
  (slugRes, slugView) <- mopt (slugFreeField slugLen freeIdRes urlpath) slugFreeFieldSet (unFreeSlug <$> free)
  (cateRes, cateView) <- mopt (selectFieldList cateLst) cateSelectFieldSet $ (int64ToInt <$>) `fmap` (unFreeCateId <$> free)
  (inpTypRes, _) <- mreq (radioFieldList inpTyp) inputTypeRadioFieldSet (unFreeInputType <$> free)
  (stsTypRes, stsTypView) <- mreq (radioFieldList stsTyp) statusTypeRadioFieldSet (unFreeStatus <$> free)
  (tagRes, tagView) <- mopt (tagField tagLen tagOneLen) tagFieldSet (unFreeTags <$> free)
  (dscRes, dscView) <- mopt (descriptionField dscLen)
                           descriptionFieldSet (Just (Textarea <$> (join (unFreeDescription <$> free))))
  (kwdRes, kwdView) <- mopt (keywordsField kwdLen) keywordsFieldSet (unFreeKeywords <$> free)
  (robRes, robView) <- mopt (robotsField robLen) robotsFieldSet(Just robots)
  (ogImgRes, ogImgView) <- mopt (ogImgField ogImgLen) ogImgFieldSet (unFreeOgImg <$> free)
  (ogTtlRes, ogTtlView) <- mopt (ogTitleField ogTtlLen) ogTitleFieldSet (unFreeOgTitle <$> free)
  (ogUrlRes, ogUrlView) <- mopt (ogUrlField ogUrlLen) ogUrlFieldSet (unFreeOgUrl <$> free)
  (ogSnmRes, ogSnmView) <- mopt (ogSiteNameField ogSnmLen) ogSiteNameFieldSet (unFreeOgSiteName <$> free)
  (odDscRes, odDscView) <- mopt (ogDescField ogDscLen) ogDescFieldSet (unFreeOgDesc <$> free)
  (odPgtRes, odPgtView) <- mopt (ogPageTypeField ogPgtLen) ogPageTypeFieldSet (Just ogpgt)
  (verRes, verView) <- mreq hiddenIdField versionFieldSet (Just version)
  let formParam = FreeForm
                  <$> freeIdRes
                  <*> titleRes
                  <*> (unTextarea <$> contRes)
                  <*> slugRes
                  <*> cateRes
                  <*> initFormUrlpath slugRes urlpath
                  <*> (liftM unTextarea <$> cssRes)
                  <*> inpTypRes
                  <*> stsTypRes
                  <*> tagRes
                  <*> ((fmap . fmap) unTextarea  dscRes)
                  <*> kwdRes
                  <*> robRes
                  <*> ogImgRes
                  <*> ogTtlRes
                  <*> ogUrlRes
                  <*> ogSnmRes
                  <*> odDscRes
                  <*> odPgtRes
                  <*> verRes
      widget = $(whamletFile "templates/freepage_form.hamlet")
  return (formParam, widget)
