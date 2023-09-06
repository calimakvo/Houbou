{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Forms.PostForm (
    postForm
  ) where

import Import
import qualified Data.Map.Ordered as O
import Forms.FormValid
import Forms.CommonForm
import DataTypes.HoubouType
import Libs.Common
import Libs.CommonWidget

postForm ::
  Maybe Post
  -> O.OMap Int [Cate]
  -> Html
  -> MForm Handler (FormResult PostForm, Widget)
postForm post cate extra = do
  master <- getYesod
  t <- liftIO getTm
  let titleLen = appPostTitleMaxLength $ appSettings master
      bodyLen = appPostTextMaxLength $ appSettings master
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
      postId = fromMaybe 0 (unPostId <$> post)
      robots = fromMaybe (Just "index,follow") (unPostRobots <$> post)
      ogpgt = fromMaybe (Just "blog") (unPostOgPageType <$> post)
      version = fromMaybe 0 (unPostVersion <$> post)
      selInpTyp = fromMaybe 1 (unPostInputType <$> post)
      selStsTyp = fromMaybe (fromEnum UnPublished) (unPostStatus <$> post)
      urlpath = if postId > 0 then join (unPostUrlpath <$> post) else (Just $ toUrlPath t)
  inpTyp <- liftHandler getInputTypeTouple
  stsTyp <- liftHandler getStatusSelectTouple
  cateLst <- liftHandler $ cateToOption <$> cateMapToTouble cate
  (postIdRes, postIdView) <- mreq hiddenField postIdFieldSet (Just postId)
  (titleRes, titleView) <- mreq (titleField titleLen) titleFieldSet (unPostTitle <$> post)
  (textRes, textView) <- mreq (bodyField bodyLen) bodyFieldSet (Textarea <$> (unPostContent <$> post))
  (slugRes, slugView) <- mopt (slugPostField slugLen postIdRes urlpath) slugPostFieldSet (unPostSlug <$> post)
  (cateRes, cateView) <- mopt (selectField $ return cateLst) cateSelectFieldSet $ (int64ToInt <$>) `fmap` (unPostCateId <$> post)
  (inpTypRes, inpTypView) <- mreq (radioFieldList inpTyp) inputTypeRadioFieldSet (unPostInputType <$> post)
  (stsTypRes, stsTypView) <- mreq (radioFieldList stsTyp) statusTypeRadioFieldSet (unPostStatus <$> post)
  (tagRes, tagView) <- mopt (tagField tagLen tagOneLen) tagFieldSet (unPostTags <$> post)
  (dscRes, dscView) <- mopt (descriptionField dscLen)
                           descriptionFieldSet (Just (Textarea <$> (join (unPostDescription <$> post))))
  (kwdRes, kwdView) <- mopt (keywordsField kwdLen) keywordsFieldSet (unPostKeywords <$> post)
  (robRes, robView) <- mopt (robotsField robLen) robotsFieldSet (Just robots)
  (ogImgRes, ogImgView) <- mopt (ogImgField ogImgLen) ogImgFieldSet (unPostOgImg <$> post)
  (ogTtlRes, ogTtlView) <- mopt (ogTitleField ogTtlLen) ogTitleFieldSet (unPostOgTitle <$> post)
  (ogUrlRes, ogUrlView) <- mopt (ogUrlField ogUrlLen) ogUrlFieldSet (unPostOgUrl <$> post)
  (ogSnmRes, ogSnmView) <- mopt (ogSiteNameField ogSnmLen) ogSiteNameFieldSet (unPostOgSiteName <$> post)
  (odDscRes, odDscView) <- mopt (ogDescField ogDscLen) ogDescFieldSet (unPostOgDesc <$> post)
  (odPgtRes, odPgtView) <- mopt (ogPageTypeField ogPgtLen) ogPageTypeFieldSet (Just ogpgt)
  (verRes, verView) <- mreq hiddenIdField versionFieldSet (Just version)
  let formParam = PostForm
                  <$> postIdRes
                  <*> titleRes
                  <*> (unTextarea <$> textRes)
                  <*> slugRes
                  <*> cateRes
                  <*> initFormUrlpath slugRes urlpath
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
      widget = $(whamletFile "templates/post_form.hamlet")
  return (formParam, widget)
