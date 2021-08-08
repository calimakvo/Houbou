{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.CateNew where

import Import
import DataTypes.HoubouType
import UrlParam.CateId
import Libs.Common
import Libs.Mapper
import Forms.CategoryForm
import Service.Category

getCateNewR :: CateId -> Handler Html
getCateNewR (CateId cate) = do
  msg <- getMessages
  let cateSet = case cate of
                  Just cateId -> Just (CateSetting 0 (int64ToInt cateId) "" 0)
                  Nothing -> Just (CateSetting 0 0 "" 0)
  (cateFormWidget, _) <- generateFormPost $ (categoryForm cateSet)
  defaultLayout $ do
    setTitle title
    $(widgetFile "categorysetting")

postCateNewR :: CateId -> Handler Html
postCateNewR _ = do
  msg <- getMessages
  ((res, cateFormWidget), _) <- runFormPost $ categoryForm Nothing
  case res of
    FormSuccess form -> do
      result <- registerCategory $ categoryFormToCateSetting form
      case result of
        Right cid -> do
          addMessage successKey "登録完了しました"
          redirect $ CateModR (CateId $ Just cid)
        Left err -> do
          $(logError) $ "postPostNewR: Post register failure" <> toText err
          addMessage errorKey "登録失敗しました"
          defaultLayout $ do
            setTitle title
            $(widgetFile "categorysetting")
    _ -> do
      defaultLayout $ do
        setTitle title
        $(widgetFile "categorysetting")

title :: Html
title = "カテゴリ"
