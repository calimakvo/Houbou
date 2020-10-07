{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.User where

import Import
import UrlParam.UserId
import Libs.Common
import Libs.CommonWidget
import Libs.Mapper
import Forms.UserForm
import Service.Common
import Service.User

getUserR ::
  UserId
  -> Handler Html
getUserR userId = do
  let uId = fromMaybe 0 (_unUserId userId)
  (usrKey, _) <- requireAuthPair
  msg <- getMessages
  chk <- eitherToBool =<< checkAdminUserPerm usrKey
  case chk of
    False -> genError "管理権限がありません"
    True -> do
      result <- getUser $ toTblUserKey uId
      case result of
        Left _ -> genError "ユーザーが見つかりません"
        Right user -> do
          (userFormWidget, _) <- generateFormPost $ (userForm $ Just user)
          defaultLayout $ do
            setTitle "ユーザー情報登録・変更"
            $(widgetFile "user")

postUserR ::
  UserId
  -> Handler Html
postUserR userId = do
  msg <- getMessages
  (usrKey, _) <- requireAuthPair
  chk <- eitherToBool =<< checkAdminUserPerm usrKey
  case chk of
    False -> redirect $ UserR userId
    True -> do
      ((res, userFormWidget), _) <- runFormPost $ userForm Nothing
      case res of
       FormSuccess form -> do
         result <- updateUser (userFormToForm form)
         case result of
           Right _ -> addMessage successKey "更新完了しました"
           Left err -> do
             $(logError) $ "postUserR: update tbl_user failure err/userId="
               <> (toText err) <> "/" <> (toText userId)
             addMessage errorKey "更新失敗しました"
         redirect $ UserR userId
       _ -> defaultLayout $ do
         setTitle "ユーザー情報登録・変更"
         $(widgetFile "user")
