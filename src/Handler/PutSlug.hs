{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.PutSlug where

import Import
import Data.Maybe
import UrlParam.PostId
import UrlParam.YPath
import UrlParam.MPath
import UrlParam.DPath
import UrlParam.Slug
import DataTypes.HoubouType
import Libs.Common
import Service.Post
import Service.Html

getPutSlugR ::
  YPath
  -> MPath
  -> DPath
  -> Slug
  -> Handler Html
getPutSlugR (YPath ypath) (MPath mpath) (DPath dpath) (Slug slug) = do
  let params = [ ypath, mpath, dpath, slug ]
      chk = all isJust params
  if chk == False
  then
    notFound
  else
    putPostSlug (fromJust ypath, fromJust mpath, fromJust dpath, fromJust slug)

putPostSlug ::
  (Text, Text, Text, Text)
  -> Handler Html
putPostSlug (ypath, mpath, dpath, slug) = do
  master <- getYesod
  let setId = appBlogSettingId $ appSettings master
  result <- getPublishPostFromSlug (rmSlash $ ypath <> "/" <> mpath <> "/" <>  dpath <> "/") slug
  case result of
    Right post -> do
      let postId = PostId (Just $ unPostId post)
      res <- createBlogContents setId postId
      case res of
        Right html -> do
          _ <- accCntUp postId
          return html  
        Left _ -> notFound
    Left err -> do
      $(logInfo) $ "putPostSlug: error = " <> toText(err)
      notFound
