{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.CateMod where

import Import
import Libs.Common
import Libs.Mapper
import UrlParam.CateId
import Forms.CategoryForm
import Service.Category

getCateModR :: CateId -> Handler Html
getCateModR (CateId cate) = do
  msg <- getMessages
  let cateId = maybe 0 id cate
  result <- getCategory cateId
  case result of
    Right cid -> do
      (cateFormWidget, _) <- generateFormPost $ categoryForm $ (Just $ toCatetoCateSet cid)
      defaultLayout $ do
        setTitle title
        $(widgetFile "categorysetting")
    Left err -> do
      $(logError) $ "getCateModR: Category not found." <> toText err
      notFound

postCateModR :: CateId -> Handler Html
postCateModR _ = do
  msg <- getMessages
  ((res, cateFormWidget), _) <- runFormPost $ categoryForm Nothing
  case res of
    FormSuccess form -> do
      result <- updateCatefory (categoryFormToCateSetting form)
      case result of
        Right cid -> do
          addMessage successKey "更新完了しました"
          redirect $ CateModR (CateId $ Just cid)
        Left err -> do
          $(logError) $ "postCateModR: update failure err/cateId="
            <> (toText err) <> "/" <> (toText $ unCategoryFormId form)
          addMessage errorKey "更新失敗しました"
          redirect $ CateModR (CateId $ Just $ unCategoryFormId form)
    _ -> do
      defaultLayout $ do
        setTitle title
        $(widgetFile "categorysetting")

title :: Html
title = "カテゴリ"
