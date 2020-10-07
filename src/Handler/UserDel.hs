{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.UserDel where

import Import
import DataTypes.HoubouType
import UrlParam.Page
import Forms.UserDelForm
import Libs.Common
import Service.Common
import Service.User

postUserDelR ::
  Handler Html
postUserDelR = do
  ((res, _), _) <- runFormPost userDelForm
  case res of
    FormSuccess (UserDelForm userId version) -> do
      result <- deleteUser True (toTblUserKey userId) (RecVersion version)
      case result of
        Right _ -> addMessage successKey "削除完了しました"
        Left err -> do
          $(logError) $ "postUserDelR: User delete failure err/user_id = " <> (toText err) <> "/" <> (toText userId)
          addMessage errorKey "削除失敗しました"
    _ -> addMessage errorKey "削除パラメータエラー"
  redirect $ UserListR (Page Nothing)
