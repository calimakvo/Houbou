{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.FreeFrameChange where

import Import
import DataTypes.HoubouType
import UrlParam.Page
import Libs.Common
import Forms.FreeFrameChgForm
import Service.Common
import Service.FreeFrame

postFreeFrameChangeR ::
  Handler Html
postFreeFrameChangeR = do
  ((res, _), _) <- runFormPost freeFrameChgForm
  case res of
    FormSuccess (FreeFrameChgForm frameId version) -> do
      result <- updateFreeFrameValidFlag (toTblFreeFrameKey frameId) (RecVersion version)
      case result of
        Right _ -> addMessage successKey "更新完了しました"
        Left err -> do
          $(logError) $ "Free Frame valid flag change failure: " <> (toText err)
          addMessage errorKey "更新失敗しました"
    _ -> addMessage errorKey "パラメータエラー"
  redirect $ FreeFrameListR (Page Nothing)
