{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.MediaNew where

import Import
import DataTypes.HoubouType
import Libs.Common
import Libs.CommonWidget
import Service.BlogSetting
import Service.Media

getMediaNewR ::
  Handler Html
getMediaNewR = do
  master <- getYesod
  let tokenKey = defaultCsrfParamName
      titleLen = appMediaTitleMaxLength $ appSettings master
  token <- getRequest >>= createCsrfToken
  defaultLayout $ do
    setTitle title
    $(widgetFile "media")

postMediaNewR ::
  Handler TypedContent
postMediaNewR = do
  master <- getYesod
  (usrKey, _) <- requireAuthPair
  let setId = appBlogSettingId $ appSettings master
      titleLen = appMediaTitleMaxLength $ appSettings master
  setting <- getBlogSetting setId
  files <- lookupFiles "file"
  titles <- (take titleLen) <$> lookupPostParams "media_title"
  if length files == 0 then
    hrespJson (fromEnum Failure) "File not found."
  else
    do
      result <- registMedia usrKey setting titles files
      case result of
        Right _ -> hrespJson (fromEnum Success) ""
        Left err -> do
          $(logError) $ "Post medianew failure err = " <> (toText err)
          hrespJson (fromEnum Failure) (toText err)

hrespJson ::
  Int
  -> Text
  -> Handler TypedContent
hrespJson res msg = selectRep $ provideRep $ return ( object [ "result" .= res, "msg" .= msg ])

title :: Html
title = "メディアアップロード"

