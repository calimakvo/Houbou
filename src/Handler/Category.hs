{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Category where

import Import
import Libs.Common
import Libs.CommonWidget

getCategoryR :: Handler Html
getCategoryR = do
  let tokenKey = defaultCsrfParamName
  token <- getRequest >>= createCsrfToken
  msg <- getMessages
  defaultLayout $ do
    setTitle title
    $(widgetFile "category")

title :: Html
title = "カテゴリ管理"
