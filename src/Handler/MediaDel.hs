{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.MediaDel where

import Import
import DataTypes.HoubouType
import Libs.Common
import UrlParam.Page
import Forms.MediaDelForm
import Service.Common
import Service.Media
import Service.BlogSetting

postMediaDelR ::
  Handler Html
postMediaDelR = do
  master <- getYesod
  let setId = appBlogSettingId $ appSettings master
  setting <- getBlogSetting setId
  ((res, _), _) <- runFormPost mediaDelForm
  case res of
    FormSuccess (MediaDelForm mediaId version) -> do
      r <- deleteMedia (toTblMediaKey mediaId) setting (RecVersion version)
      case r of
        Right _ -> addMessage successKey "削除完了しました"
        Left err -> do
          $(logError) $ "Media delete failure err/frame_id = "
            <> (toText err) <> "/" <> (toText mediaId)
          addMessage errorKey "削除失敗しました"
    _ -> addMessage errorKey "削除パラメータエラー"
  redirect $ MediaListR (Page Nothing)
