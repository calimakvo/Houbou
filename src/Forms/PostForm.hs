{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.PostForm (
    postForm
  , PostForm(..)
  ) where

import Import
import Forms.FormValid
import Forms.CommonForm
import DataTypes.HoubouType
import UrlParam.Page
import Libs.Common

data PostForm = PostForm {
    unPostFormId :: Int64
  , unPostFormTitle :: Text
  , unPostFormContent :: Text
  , unPostFormSlug :: Maybe Text
  , unPostFormUrlpath :: Maybe Text
  , unPostFormInputType :: Int
  , unPostFormStatus :: Int
  , unPostFormTags :: Maybe Text
  , unPostFormVersion :: Int
} deriving(Eq, Show)

postForm ::
  Maybe Post
  -> Html
  -> MForm Handler (FormResult PostForm, Widget)
postForm post extra = do
  master <- getYesod
  backListPageType <- lookupSession "truePageType"
  t <- liftIO getTm
  let titleLen = appPostTitleMaxLength $ appSettings master
      bodyLen = appPostTextMaxLength $ appSettings master
      slugLen = appSlugTextMaxLength $ appSettings master
      tagLen = appTagMaxLength $ appSettings master
      tagOneLen = appTagOneMaxLength $ appSettings master
      postId = fromMaybe 0 (unPostId <$> post)
      version = fromMaybe 0 (unPostVersion <$> post)
      selInpTyp = fromMaybe 1 (unPostInputType <$> post)
      selStsTyp = fromMaybe (fromEnum UnPublished) (unPostStatus <$> post)
      urlpath = if postId > 0 then join (unPostUrlpath <$> post) else (Just $ toUrlPath t)
  inpTyp <- liftHandler getInputTypeTouple
  stsTyp <- liftHandler getStatusSelectTouple
  (postIdRes, postIdView) <- mreq hiddenField postIdFieldSet (Just postId)
  (titleRes, titleView) <- mreq (titleField titleLen) titleFieldSet (unPostTitle <$> post)
  (textRes, textView) <- mreq (bodyField bodyLen) bodyFieldSet (Textarea <$> (unPostContent <$> post))
  (slugRes, slugView) <- mopt (slugPostField slugLen postIdRes urlpath) slugPostFieldSet (unPostSlug <$> post)
  (inpTypRes, inpTypView) <- mreq (radioFieldList inpTyp) inputTypeRadioFieldSet (unPostInputType <$> post)
  (stsTypRes, stsTypView) <- mreq (radioFieldList stsTyp) statusTypeRadioFieldSet (unPostStatus <$> post)
  (tagRes, tagView) <- mopt (tagField tagLen tagOneLen) tagFieldSet (unPostTags <$> post)
  (verRes, verView) <- mreq hiddenIdField versionFieldSet (Just version)
  let formParam = PostForm
                  <$> postIdRes
                  <*> titleRes
                  <*> (unTextarea <$> textRes)
                  <*> slugRes
                  <*> initFormUrlpath slugRes urlpath
                  <*> inpTypRes
                  <*> stsTypRes
                  <*> tagRes
                  <*> verRes
      widget = do
        $(whamletFile "templates/post_form.hamlet")
  return (formParam, widget)
