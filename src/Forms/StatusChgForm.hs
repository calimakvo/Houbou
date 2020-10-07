{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.StatusChgForm (
    statusChgForm
  , StatusChgForm(..)
  ) where

import Import
import Forms.FormValid
import Forms.CommonForm

data StatusChgForm = StatusChgForm {
    unStChgFormId :: Int64
  , unStChgFormVersion :: Int
}

statusChgForm :: Html -> MForm Handler (FormResult StatusChgForm, Widget)
statusChgForm _ = do
  (statusIdRes, _) <- mreq hiddenId64Field postIdFieldSet Nothing
  (versionRes, _) <- mreq hiddenIdField versionFieldSet Nothing
  let formParam = StatusChgForm
                    <$> statusIdRes
                    <*> versionRes
      widget = return ()
  return (formParam, widget)
