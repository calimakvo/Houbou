{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.FrameChange where

import Import
import DataTypes.HoubouType
import Forms.FrameChgForm
import UrlParam.Page
import Libs.Common
import Service.Common
import Service.Frame

postFrameChangeR ::
  Handler Html
postFrameChangeR = do
  ((res, _), _) <- runFormPost frameChgForm
  case res of
    FormSuccess (FrameChgForm frameId version) -> do
      result <- updateFrameValidFlag (toTblFrameKey frameId) (RecVersion version)
      case result of
        Right _ -> addMessage successKey "更新完了しました"
        Left err -> do
          $(logError) $ "postFrameChangeR: Status update failure err/frameId="
            <> toText err <> "/" <> toText frameId
          addMessage errorKey "更新失敗しました"
    _ -> addMessage errorKey "パラメータエラー"
  redirect $ FrameListR (Page Nothing)
