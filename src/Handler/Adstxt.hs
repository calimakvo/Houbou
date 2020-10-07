{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Adstxt where

import Import
import DataTypes.HoubouType
import Libs.CommonWidget
import Service.BlogSetting

getAdstxtR :: Handler TypedContent
getAdstxtR = do
  master <- getYesod
  let setId = appBlogSettingId $ appSettings master
  setting <- getBlogSetting setId
  case (unBlogSettingAdstxt setting) of
    Just txt -> retPlain (unpack txt)
    Nothing -> notFound
