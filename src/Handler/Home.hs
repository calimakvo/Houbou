{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home where

import Import
import UrlParam.PostId
import Service.Post
import Service.Html

getHomeR ::
  Handler Html
getHomeR = do
  master <- getYesod
  let setId = appBlogSettingId $ appSettings master
  res <- createBlogContents setId (PostId Nothing)
  case res of
    Right html -> do
      _ <- accCntUp (PostId Nothing)
      return html
    Left _ -> notFound
