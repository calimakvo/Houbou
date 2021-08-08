{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.CategoryDelForm (
    categoryDelForm
  , CategoryDelForm(..)
  ) where

import Import
import Data.Maybe
import DataTypes.HoubouType
import Forms.FormValid
import Forms.CommonForm

data CategoryDelForm = CategoryDelForm {
    unCategoryDelFormId :: Int64
  , unCategoryDelFormVersion :: Int
} deriving(Eq, Show)

categoryDelForm ::
  Maybe CateUseStatus
  -> (Maybe Cate)
  -> Html
  -> MForm Handler (FormResult CategoryDelForm, Widget)
categoryDelForm state cate extra = do
  (cateIdRes, cateIdView) <- mreq hiddenId64Field cateIdFieldSet (unCateId <$> cate)
  (verRes, verView) <- mreq hiddenIdField versionFieldSet (unCateVer <$> cate)
  let formParam = CategoryDelForm
                    <$> cateIdRes
                    <*> verRes
      widget = $(whamletFile "templates/categorydel_form.hamlet")
  return (formParam, widget)
