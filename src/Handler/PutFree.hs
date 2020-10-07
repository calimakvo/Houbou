{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.PutFree where

import Import
import UrlParam.FreeId
import Service.Free
import Service.Html

getPutFreeR ::
  FreeId
  -> Handler Html
getPutFreeR freeId = do
  master <- getYesod
  let setId = appBlogSettingId $ appSettings master
  res <- createBlogFreeContents setId freeId
  case res of
    Right html -> do
      _ <- accFreeCntUp freeId
      return html
    Left _ -> notFound

