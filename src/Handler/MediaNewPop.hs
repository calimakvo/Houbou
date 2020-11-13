{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.MediaNewPop where

import Import
import Libs.CommonWidget
import UrlParam.Page

getMediaNewPopR :: Handler Html
getMediaNewPopR = do
  master <- getYesod
  let tokenKey = defaultCsrfParamName
      titleLen = appMediaTitleMaxLength $ appSettings master
  token <- getRequest >>= createCsrfToken
  genericLayout $ do
    setTitle title
    $(widgetFile "mediapop")

title :: Html
title = "メディアアップロード"
