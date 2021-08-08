{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.CateDel where

import Import
import DataTypes.HoubouType
import Libs.Common
import UrlParam.CateId
import Forms.CategoryDelForm
import Service.Category

getCateDelR ::
  CateId
  -> Handler Html
getCateDelR (CateId cate) = do
  msg <- getMessages
  let cateId = maybe 0 id cate
  res <- checkCategoryUse cateId
  result <- getCategory cateId
  case result of
    Right cid -> do
      (cateFormWidget, _) <- generateFormPost $ categoryDelForm (Just res) (Just cid)
      defaultLayout $ do
        setTitle title
        $(widgetFile "categorydel")
    Left _ -> do
      addMessage errorKey "カテゴリの取得に失敗しました"
      redirect CategoryR

postCateDelR ::
  CateId
  -> Handler Html
postCateDelR _ = do
  ((res, _), _) <- runFormPost $ categoryDelForm Nothing Nothing
  case res of
    FormSuccess (CategoryDelForm cateId ver) -> do
      result <- deleteCatefory cateId ver
      case result of
        Right _ -> do
          addMessage successKey "削除が完了しました"
          redirect CategoryR
        Left ErrRecNotFound -> do
          addMessage errorKey "削除対象のカテゴリが存在しません"
          redirect CategoryR
        Left ErrRecVersion -> do
          addMessage errorKey "他のユーザーによって更新されています、再読み込みしてください"
          redirect CategoryR
        Left err -> do
          $(logError) $ "postCateDelR: Category delete error :" <> toText err
          addMessage errorKey "削除エラーです"
          redirect CategoryR
    FormFailure _ -> do
      addMessage errorKey "カテゴリ情報の取得に失敗しました"
      redirect CategoryR
    _ -> redirect CategoryR

title :: Html
title = "カテゴリ削除"
