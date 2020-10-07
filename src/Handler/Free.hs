{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Free where

import Import
import DataTypes.HoubouType
import Forms.FreeForm
import UrlParam.FreeId
import Libs.Common
import Libs.Mapper
import Service.BlogSetting
import Service.Free
import Service.Tags

getFreeR ::
  FreeId
  -> Handler Html
getFreeR freeId = do
  master <- getYesod
  let setId = appBlogSettingId $ appSettings master
  msg <- getMessages
  setting <- getBlogSetting setId
  result <- getFreeFromId freeId
  case result of
    Left err -> do
      $(logError) $ "getFreeR: record not found." <> toText err
      notFound
    Right free -> do
      (freeWidget, _) <- generateFormPost $ (freeForm $ Just free)
      defaultLayout $ do
        setTitle title
        $(widgetFile "free")

postFreeR ::
  FreeId
  -> Handler Html
postFreeR freeId = do
  master <- getYesod
  let setId = appBlogSettingId $ appSettings master
  msg <- getMessages
  setting <- getBlogSetting setId
  ((res, freeWidget), _) <- runFormPost $ freeForm Nothing
  case res of
   FormSuccess form -> do
     let free = freeFormToFree form
     result <- updateFree free
     case result of
       Right fid -> do
         _ <- case unFreeTags free of
                Just tags -> do
                  _ <- updateFreeTagList fid tags
                  return ()
                Nothing -> return ()
         addMessage successKey "更新完了しました"
       Left err -> do
         $(logError) $ "postFreeR: free page update failure err/freeId="
           <> toText err <> "/" <> toText (unFreeFormId form)
         addMessage errorKey "更新失敗しました"
     redirect $ FreeR freeId
   _ -> do
     result <- getFreeFromId freeId
     case result of
       Left err -> do
         $(logError) $ "getFreeR: record not found." <> toText err
         notFound
       Right free ->
         defaultLayout $ do
           setTitle title
           $(widgetFile "free")

title :: Html
title = "フリーページ編集"
