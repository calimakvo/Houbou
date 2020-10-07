{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Put where

import Import
import UrlParam.PostId
import Service.Post
import Service.Html

getPutR ::
  PostId
  -> Handler Html
getPutR postId = do
  master <- getYesod
  let setId = appBlogSettingId $ appSettings master
  res <- createBlogContents setId postId
  case res of
    Right html -> do
      _ <- accCntUp postId
      return html
    Left _ -> notFound
