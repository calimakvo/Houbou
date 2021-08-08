{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.ListSearchSelectForm (
    listSearchSelectForm
  , ListSearchSelectForm(..)
  ) where

import Import
import DataTypes.HoubouType
import Forms.CommonForm

listSearchSelectForm ::
  Maybe SearchParam -> Html -> MForm Handler (FormResult ListSearchSelectForm, Widget)
listSearchSelectForm p extra = do
  master <- getYesod
  let xs = appPagePerLineSelector $ appSettings master
  selector <- liftHandler $ lineSelectTouple xs
  (selectRes, serectView) <- mreq (selectFieldList selector) lineSelectFieldSet (unSearchParamPagePerLine <$> p)
  (pageRes, pageView) <- mreq hiddenField pageNumFieldSet (Just 1)
  (stypeRes, stypeView) <- mreq hiddenField searchTypeFieldSet (fromEnum <$> unSearchParamSearchType <$> p)
  (cateRes, cateView) <- mopt hiddenField cateIdFieldSet (unSearchParamCateId <$> p)
  let formParam = ListSearchSelectForm <$> selectRes
                                       <*> pageRes
                                       <*> stypeRes
                                       <*> cateRes
      widget = $(whamletFile "templates/list_search_select_form.hamlet")
  return (formParam, widget)
