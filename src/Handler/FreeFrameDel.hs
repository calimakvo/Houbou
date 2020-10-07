{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.FreeFrameDel where

import Import
import DataTypes.HoubouType
import Libs.Common
import Forms.FreeFrameDelForm
import UrlParam.Page
import Service.Common
import Service.FreeFrame

postFreeFrameDelR ::
  Handler Html
postFreeFrameDelR = do
  ((res, _), _) <- runFormPost freeFrameDelForm
  case res of
    FormSuccess (FreeFrameDelForm frameId version) -> do
      result <- deleteFreeFrame (toTblFreeFrameKey frameId) (RecVersion version)
      case result of
        Right _ -> addMessage successKey "削除完了しました"
        Left err -> do
          $(logError) $ "FreeFrame delete failure err/frame_id = " <> (toText err) <> "/" <> (toText frameId)
          addMessage errorKey "削除失敗しました"
    _ -> addMessage errorKey "削除パラメータエラー"
  redirect $ FreeFrameListR (Page Nothing)
