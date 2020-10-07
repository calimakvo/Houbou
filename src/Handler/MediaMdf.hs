{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.MediaMdf where

import Import
import UrlParam.MediaId
import Forms.MediaMdfForm
import Libs.Common
import Libs.Mapper
import Service.BlogSetting
import Service.Media

getMediaMdfR ::
  MediaId
  -> Handler Html
getMediaMdfR mediaId = do
  master <- getYesod
  msg <- getMessages
  let setId = appBlogSettingId $ appSettings master
  setting <- getBlogSetting setId
  record <- getMediaFromId mediaId
  case record of
    Left _ -> notFound
    Right media -> do
      (postWidget, _) <- generateFormPost $ (mediaMdfForm setting (Just media))
      defaultLayout $ do
        setTitle title
        $(widgetFile "mediamdf")

postMediaMdfR ::
  MediaId
  -> Handler Html
postMediaMdfR mediaId = do
  master <- getYesod
  msg <- getMessages
  let setId = appBlogSettingId $ appSettings master
  setting <- getBlogSetting setId
  ((res, postWidget), _) <- runFormPost $ mediaMdfForm setting Nothing
  case res of
   FormSuccess form -> do
     result <- updateMedia (mediaFormToMedia form)
     case result of
       Right _ -> addMessage successKey "更新完了しました"
       Left err -> do
         $(logError) $ "postMediaMdfR: update failure err/postId="
           <> (toText err) <> "/" <> (toText $ unMediaMdfFormId form)
         addMessage errorKey "更新失敗しました"
     redirect $ MediaMdfR mediaId
   _ -> defaultLayout $ do
     setTitle title
     $(widgetFile "mediamdf")

title :: Html
title = "メディア編集"
