{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.PostPrev where

import Import
import DataTypes.HoubouType
import Libs.Common
import Libs.CommonWidget
import Libs.Mapper
import Libs.Template
import Forms.PrevForm
import Service.Html
import Service.Preview

getPostPrevR :: Handler Html
getPostPrevR = do
  master <- getYesod
  form <- lookupSession prevpostform >>= chkForm
  let prev = toPrevForm form
      defPost = prevFormToPost prev
      contents = maybeToText $ unPrevFormPreviewContent prev
      setId = appBlogSettingId $ appSettings master
  chk <- liftIO $ checkTemplate (unpack contents)
  case chk of
    Nothing -> do
      post <- createPrevPost defPost
      res <- createPrevBlogContents setId (PostInf post [])
      case res of
        Right html -> return html
        Left _ -> return "プレビューできません"
    Just err -> return $ toHtml (errPrevText err)

postPostPrevR ::
  Handler TypedContent
postPostPrevR = do
  ((res, _), _) <- runFormPost prevForm
  case res of
    FormSuccess prev -> do
      setSession prevpostform $ encodePrevFormParm prev
      retJson (fromEnum Success) 0 ""
    _ -> retJson (fromEnum Failure) 0 "プレビューエラー"

title :: Html
title = "プレビュー"
