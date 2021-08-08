{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.CategoryForm (
    toCatetoCateSet
  , categoryForm
  , CategoryForm(..)
  ) where

import Import
import DataTypes.HoubouType
import Forms.FormValid
import Forms.CommonForm

data CategoryForm = CategoryForm {
    unCategoryFormId :: Int64
  , unCategoryFormParentId :: Int
  , unCategoryFormName :: Text
  , unCategoryFormVersion :: Int
} deriving(Eq, Show)

categoryForm ::
  (Maybe CateSetting)
  -> Html
  -> MForm Handler (FormResult CategoryForm, Widget)
categoryForm cate extra = do
  master <- getYesod
  let cateNameLen = appCateNameMaxLength $ appSettings master
  (cateIdRes, cateIdView) <- mreq hiddenId64Field cateIdFieldSet (unCateSettingId <$> cate)
  (catePidRes, catePidView) <- mreq hiddenIdField catePidFieldSet (unCateSettingParentId <$> cate)
  (nameRes, nameView) <- mreq (cateNameField cateNameLen) cateNameFieldSet (unCateSettingName <$> cate)
  (verRes, verView) <- mreq hiddenIdField versionFieldSet (unCateSettingVersion <$> cate)
  let formParam = CategoryForm
                    <$> cateIdRes
                    <*> catePidRes
                    <*> nameRes
                    <*> verRes
      widget = $(whamletFile "templates/category_form.hamlet")
  return (formParam, widget)

toCatetoCateSet ::
  Cate
  -> CateSetting
toCatetoCateSet cate =
  CateSetting {
    unCateSettingId = unCateId cate
  , unCateSettingParentId = maybe 0 id (unCatePid cate)
  , unCateSettingName = unCateName cate
  , unCateSettingVersion = unCateVer cate
  }
