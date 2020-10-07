{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.ListSelectForm (
    listSelectForm
  , ListSelectForm(..)
  ) where

import Import
import Forms.CommonForm

data ListSelectForm = ListSelectForm {
  unPagePerLine :: Int
} deriving(Show, Eq)

listSelectForm :: (Maybe Int) -> Html -> MForm Handler (FormResult ListSelectForm, Widget)
listSelectForm ppl extra = do
  master <- getYesod
  let xs = appPagePerLineSelector $ appSettings master
  selector <- liftHandler $ lineSelectTouple xs
  (selectRes, serectView) <- mreq (selectFieldList selector) lineSelectFieldSet ppl
  let formParam = ListSelectForm <$> selectRes
      widget = $(whamletFile "templates/list_select_form.hamlet")
  return (formParam, widget)
