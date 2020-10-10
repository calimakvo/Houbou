{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Post where

import Import
import DataTypes.HoubouType
import Forms.PostForm
import Libs.Common
import Libs.CommonWidget
import Libs.Mapper
import UrlParam.PostId
import Service.Post
import Service.Tags

getPostR ::
  PostId
  -> Handler Html
getPostR postId = do
  msg <- getMessages
  result <- getPostFromId postId
  case result of
    Left err -> do
      $(logError) $ "getPostR: record not found." <> toText err
      notFound
    Right post -> do
      (postWidget, _) <- generateFormPost $ (postForm $ Just post)
      defaultLayout $ do
        setTitle title
        $(widgetFile "post")

postPostR ::
  PostId
  -> Handler Html
postPostR postId = do
  msg <- getMessages
  ((res, postWidget), _) <- runFormPost $ postForm Nothing
  case res of
   FormSuccess form -> do
     let post = postFormToPost form
     result <- updatePost post
     case result of
       Right pid -> do
         _ <- case unPostTags post of
                Just tags -> do
                  _ <- updatePostTagList pid tags
                  return ()
                Nothing -> return ()
         addMessage successKey "更新完了しました"
       Left err -> do
         $(logError) $ "postPostR: update failure err/postId="
           <> (toText err) <> "/" <> (toText $ unPostFormId form)
         addMessage errorKey "更新失敗しました"
     redirect $ PostR postId
   _ -> do
     result <- getPostFromId postId
     case result of
       Left err -> do
         $(logError) $ "getPostR: record not found." <> toText err
         notFound
       Right post ->
         defaultLayout $ do
           setTitle title
           $(widgetFile "post")

title :: Html
title = "投稿"
