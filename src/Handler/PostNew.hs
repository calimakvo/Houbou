{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.PostNew where

import Import
import DataTypes.HoubouType
import Libs.Common
import Libs.CommonWidget
import UrlParam.PostId
import Forms.PostForm
import Libs.Mapper
import Service.Post
import Service.Tags

getPostNewR ::
  Handler Html
getPostNewR = do
  msg <- getMessages
  let tokenKey = defaultCsrfParamName
  token <- getRequest >>= createCsrfToken
  (postWidget, _) <- generateFormPost $ (postForm Nothing)
  defaultLayout $ do
    setTitle "投稿"
    $(widgetFile "postnew")

postPostNewR ::
  Handler Html
postPostNewR = do
  msg <- getMessages
  let tokenKey = defaultCsrfParamName
  token <- getRequest >>= createCsrfToken
  (usrKey, _) <- requireAuthPair
  ((res, postWidget), _) <- runFormPost $ postForm Nothing
  case res of
    FormSuccess form -> do
      let post = postFormToPost form
      result <- registerPost usrKey post
      case result of
        Right pid -> do
          _ <- case unPostTags post of
                 Just tags -> do
                   _ <- updatePostTagList pid tags
                   return ()
                 Nothing -> return ()
          addMessage successKey "登録完了しました"
          redirect $ PostR (PostId $ Just pid)
        Left err -> do
          $(logError) $ "postPostNewR: Post register failure" <> toText err
          addMessage errorKey "登録失敗しました"
          defaultLayout $ do
            setTitle "投稿"
            $(widgetFile "postnew")
    _ -> do
      defaultLayout $ do
        setTitle "投稿"
        $(widgetFile "postnew")
