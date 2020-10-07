{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.PostDel where

import Import
import DataTypes.HoubouType
import Libs.Common
import UrlParam.Page
import Forms.PostDelForm
import Service.Common
import Service.Post

postPostDelR ::
  Int
  -> Handler Html
postPostDelR pageType = do
  ((res, _), _) <- runFormPost postDelForm
  let (postViewStatus, _) = searchType pageType
      truePageType = fromEnum postViewStatus
  case res of
    FormSuccess (PostDelForm postId version) -> do
      result <- deletePost (toTblPostKey postId) (RecVersion version)
      case result of
        Right _ -> addMessage successKey "削除完了しました"
        Left err -> do
          $(logError) $ "Post delete failure err/post_id = " <> (toText err) <> "/" <> (toText postId)
          addMessage errorKey "削除失敗しました"
    _ -> addMessage errorKey "削除パラメータエラー"
  redirect $ PostListR truePageType (Page Nothing)
