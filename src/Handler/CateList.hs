{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.CateList where

import Import
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as A
import DataTypes.HoubouType
import Libs.Common
import Libs.CommonWidget
import Forms.CategoryListForm
import Service.Category

getCateListR :: Handler TypedContent
getCateListR = do
  cates <- getCategories
  retJsonCate (fromEnum Success) Nothing cates

postCateListR :: Handler TypedContent
postCateListR = do
  ((res, _), _) <- runFormPost categoryListForm
  case res of
    FormSuccess (CategoryListForm cate) -> do
      let encres = (A.decode $ L.fromStrict $ T.encodeUtf8 cate)::Maybe [Cate]
      case encres of
        Just cs -> do
          result <- updateCategoryList cs
          case result of
            Right _ -> do
              cates <- getCategories
              retJsonCate (fromEnum Success) Nothing cates
            Left _ -> do
              retJsonCate (fromEnum Failure) (Just "他のユーザーにより更新されているため再読み込みしてください") []
        Nothing -> do
          $(logError) $ "postCateListR: JSON encode error = " <> toText encres
          retJsonCate (fromEnum Failure) (Just "更新失敗しました") []
    _ -> do
        $(logError) "postCateListR: form error"
        retJsonCate (fromEnum Failure) (Just "フォームエラー") []
