{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Forms.SearchForm (
    searchForm
  ) where

import Import
import qualified Data.Map.Ordered as O
import Forms.CommonForm
import DataTypes.HoubouType

searchForm ::
  Maybe SearchParam
  -> O.OMap Int [Cate]
  -> Html
  -> MForm Handler (FormResult SearchForm, Widget)
searchForm p cate extra = do
  cateLst <- liftHandler $ cateMapToTouble cate
  (pageRes, pageView) <- mreq hiddenField pageNumFieldSet (Just 1)
  (stypeRes, stypeView) <- mreq hiddenField searchTypeFieldSet (fromEnum <$> unSearchParamSearchType <$> p)
  (cateSelRes, cateSelView) <- mopt (selectFieldList cateLst) cateSelectFieldSet (unSearchParamCateId <$> p)
  let formParam = SearchForm <$> pageRes
                             <*> stypeRes
                             <*> cateSelRes
      widget = $(whamletFile "templates/search_form.hamlet")
  return (formParam, widget)
