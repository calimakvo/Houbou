{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.UserNew where

import Import
import Libs.Common
import Libs.CommonWidget
import Libs.Mapper
import Forms.UserNewForm
import UrlParam.UserId
import Service.Common
import Service.User

getUserNewR ::
  Handler Html
getUserNewR = do
  msg <- getMessages
  (usrKey, _) <- requireAuthPair
  chk <- eitherToBool =<< checkAdminUserPerm usrKey
  case chk of
    False -> genError "管理権限がありません"
    True -> do
      (userFormWidget, _) <- generateFormPost $ userNewForm Nothing
      defaultLayout $ do
        setTitle "ユーザー情報登録・変更"
        $(widgetFile "user")

postUserNewR ::
  Handler Html
postUserNewR = do
  msg <- getMessages
  (usrKey, _) <- requireAuthPair
  chk <- eitherToBool =<< checkAdminUserPerm usrKey
  case chk of
    False -> genError "管理権限がありません"
    True -> do
      ((res, userFormWidget), _) <- runFormPost $ userNewForm Nothing
      case res of
       FormSuccess form -> do
         result <- registerUser (userNewFormToForm form)
         case result of
           Right userId -> do
             addMessage successKey "更新完了しました"
             redirect $ UserR $ UserId (Just userId)
           Left err -> do
             $(logError) $ "postUserNewR: register tbl_user failure err=" <> (toText err)
             addMessage errorKey "登録失敗しました"
             redirect UserNewR 
       _ -> defaultLayout $ do
         setTitle "ユーザー情報登録・変更"
         $(widgetFile "user")
