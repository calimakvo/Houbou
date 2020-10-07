{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.StatusChgFreeForm (
    statusChgFreeForm
  , StatusChgFreeForm(..)
  ) where

import Import
import Forms.FormValid
import Forms.CommonForm

data StatusChgFreeForm = StatusChgFreeForm {
    unStChgFormFreeId :: Int64
  , unStChgFormFreeVersion :: Int
}

statusChgFreeForm :: Html -> MForm Handler (FormResult StatusChgFreeForm, Widget)
statusChgFreeForm _ = do
  (statusIdRes, _) <- mreq hiddenId64Field freeIdFieldSet Nothing
  (versionRes, _) <- mreq hiddenIdField versionFieldSet Nothing
  let formParam = StatusChgFreeForm
                    <$> statusIdRes
                    <*> versionRes
      widget = return ()
  return (formParam, widget)
