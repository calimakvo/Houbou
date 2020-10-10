{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.PutFreeSlug where

import Import
import Data.Maybe
import UrlParam.FreeId
import UrlParam.YPath
import UrlParam.MPath
import UrlParam.DPath
import UrlParam.Slug
import DataTypes.HoubouType
import Libs.Common
import Service.Free
import Service.Html

getPutFreeSlugR ::
  YPath
  -> MPath
  -> DPath
  -> Slug
  -> Handler Html
getPutFreeSlugR (YPath ypath) (MPath mpath) (DPath dpath) (Slug slug) = do
  let params = [ ypath, mpath, dpath, slug ]
      chk = all isJust params
  if chk == False
  then
    notFound
  else
    putFreeSlug (fromJust ypath, fromJust mpath, fromJust dpath, fromJust slug)

putFreeSlug ::
  (Text, Text, Text, Text)
  -> Handler Html
putFreeSlug (ypath, mpath, dpath, slug) = do
  master <- getYesod
  let setId = appBlogSettingId $ appSettings master
  result <- getPublishFreeFromSlug (rmSlash $ ypath <> "/" <> mpath <> "/" <>  dpath <> "/") slug
  case result of
    Right free -> do
      let freeId = FreeId (Just $ unFreeId free)
      res <- createBlogFreeContents setId freeId
      case res of
        Right html -> do
          _ <- accFreeCntUp freeId
          return html  
        Left _ -> notFound
    Left err -> do
      $(logInfo) $ "putFreeSlug: error = " <> toText(err)
      notFound
