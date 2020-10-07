{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.UserDelForm (
    userDelForm
  , UserDelForm(..)
  ) where

import Import
import Forms.FormValid
import Forms.CommonForm

data UserDelForm = UserDelForm {
    unUserDelFormId :: Int64
  , unUserDelFormVersion :: Int
}

userDelForm :: Html -> MForm Handler (FormResult UserDelForm, Widget)
userDelForm _ = do
  (delIdRes, _) <- mreq hiddenId64Field userIdFieldSet Nothing
  (versionRes, _) <- mreq hiddenIdField versionFieldSet Nothing
  let formParam = UserDelForm
                    <$> delIdRes
                    <*> versionRes
      widget = return ()
  return (formParam, widget)
