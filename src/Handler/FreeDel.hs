{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.FreeDel where

import Import
import DataTypes.HoubouType
import Libs.Common
import Forms.FreeDelForm
import UrlParam.Page
import Service.Common
import Service.Free

postFreeDelR ::
  Int
  -> Handler Html
postFreeDelR pageType = do
  ((res, _), _) <- runFormPost freeDelForm
  let (freeViewStatus, _) = searchType pageType
      truePageType = fromEnum freeViewStatus
  case res of
    FormSuccess (FreeDelForm freeId version) -> do
      result <- deleteFree (toTblFreeKey freeId) (RecVersion version)
      case result of
        Right _ -> addMessage successKey "削除完了しました"
        Left err -> do
          $(logError) $ "Free delete failure err/free_id = " <> (toText err) <> "/" <> (toText freeId)
          addMessage errorKey "削除失敗しました"
    _ -> addMessage errorKey "削除パラメータエラー"
  redirect $ FreeListR truePageType (Page Nothing)
