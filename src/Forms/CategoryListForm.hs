{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.CategoryListForm (
    categoryListForm
  , CategoryListForm(..)
  ) where

import Import
import Forms.CommonForm

data CategoryListForm = CategoryListForm {
    unCategoryListCateList :: Text
} deriving(Eq, Show)

categoryListForm ::
  Html
  -> MForm Handler (FormResult CategoryListForm, Widget)
categoryListForm _ = do
  (catesRes, _) <- mreq textField cateListFieldSet Nothing
  let formParam = CategoryListForm
                    <$> catesRes
      widget = return ()
  return (formParam, widget)
