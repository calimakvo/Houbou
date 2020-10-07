{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.FrameDel where

import Import
import DataTypes.HoubouType
import Libs.Common
import Forms.FrameDelForm
import UrlParam.Page
import Service.Common
import Service.Frame

postFrameDelR ::
  Handler Html
postFrameDelR = do
  ((res, _), _) <- runFormPost frameDelForm
  case res of
    FormSuccess (FrameDelForm frameId version) -> do
      r <- deleteFrame (toTblFrameKey frameId) (RecVersion version)
      case r of
        Right _ -> addMessage successKey "削除完了しました"
        Left err -> do
          $(logError) $ "Frame delete failure err/frame_id = "
            <> (toText err) <> "/" <> (toText frameId)
          addMessage errorKey "削除失敗しました"
    _ -> addMessage errorKey "削除パラメータエラー"
  redirect $ FrameListR (Page Nothing)
