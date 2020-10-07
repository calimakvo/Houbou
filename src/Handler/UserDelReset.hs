{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.UserDelReset where

import Import
import DataTypes.HoubouType
import Forms.UserDelForm
import UrlParam.Page
import Libs.Common
import Service.Common
import Service.User

postUserDelResetR ::
  Handler Html
postUserDelResetR = do
  ((res, _), _) <- runFormPost userDelForm
  case res of
    FormSuccess (UserDelForm userId version) -> do
      result <- deleteUser False (toTblUserKey userId) (RecVersion version)
      case result of
        Right _ -> addMessage successKey "ユーザーの有効化が完了しました"
        Left err -> do
          $(logError) $ "postUserDelResetR: User valid failure err/user_id = "
            <> (toText err) <> "/" <> (toText userId)
          addMessage errorKey "ユーザーの有効化にしました"
    _ -> addMessage errorKey "ユーザーの有効化パラメータエラー"
  redirect $ UserListR (Page Nothing)
