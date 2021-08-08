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
import Service.Common
import Service.Free

postFreeDelR ::
  Handler Html
postFreeDelR = do
  ((res, _), _) <- runFormPost freeDelForm
  case res of
    FormSuccess (FreeDelForm freeId version) -> do
      result <- deleteFree (toTblFreeKey freeId) (RecVersion version)
      case result of
        Right _ -> addMessage successKey "削除完了しました"
        Left err -> do
          $(logError) $ "Free delete failure err/free_id = " <> (toText err) <> "/" <> (toText freeId)
          addMessage errorKey "削除失敗しました"
    _ -> addMessage errorKey "削除パラメータエラー"
  redirect $ FreeListR
