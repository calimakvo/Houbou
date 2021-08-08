{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.PutCateList where

import Import
import UrlParam.CateId
import Service.Html

getPutCateListR :: CateId -> Handler Html
getPutCateListR cateId = do
  master <- getYesod
  let setId = appBlogSettingId $ appSettings master
  res <- createCateListContents setId cateId
  case res of
    Right html -> return html
    Left _ -> notFound
