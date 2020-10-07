{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.PutTagList where

import Import
import UrlParam.TagId
import Service.Html

getPutTagListR ::
  TagId
  -> Handler Html
getPutTagListR tagId = do
  master <- getYesod
  let setId = appBlogSettingId $ appSettings master
  res <- createTagListContents setId tagId
  case res of
    Right html -> return html
    Left _ -> notFound
