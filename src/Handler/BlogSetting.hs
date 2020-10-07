{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.BlogSetting where

import Import
import Libs.Common
import Libs.Mapper
import Forms.BlogSettingForm
import Service.BlogSetting

getBlogSettingR ::
  Handler Html
getBlogSettingR = do
  master <- getYesod
  msg <- getMessages
  let setId = appBlogSettingId $ appSettings master
  setting <- getBlogSetting setId
  (settingFormWidget, _) <- generateFormPost $ (blogSettingForm $ Just setting)
  defaultLayout $ do
    setTitle title
    $(widgetFile "blogsetting")

postBlogSettingR ::
  Handler Html
postBlogSettingR = do
  master <- getYesod
  msg <- getMessages
  let setId = appBlogSettingId $ appSettings master
  ((res, settingFormWidget), _) <- runFormPost $ blogSettingForm Nothing
  case res of
   FormSuccess form -> do
     result <- updateBlogSetting setId (blogSettingFormToBlogSetting form)
     case result of
       Right _ -> addMessage successKey "更新完了しました"
       Left err -> do
         $(logError) $ "postBlogSettingR: update blog setting failure err="
           <> (toText err)
         addMessage errorKey "更新失敗しました"
     redirect BlogSettingR
   _ -> defaultLayout $ do
     setTitle title
     $(widgetFile "blogsetting")

title :: Html
title = "ブログ設定"
