{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.MediaDelForm (
    mediaDelForm
  , MediaDelForm(..)
  ) where

import Import
import Forms.FormValid
import Forms.CommonForm

data MediaDelForm = MediaDelForm {
    unMediaDelFormId :: Int64
  , unMediaDelFormVersion :: Int
}

mediaDelForm :: Html -> MForm Handler (FormResult MediaDelForm, Widget)
mediaDelForm _ = do
  (delIdRes, _) <- mreq hiddenId64Field mediaIdFieldSet Nothing
  (versionRes, _) <- mreq hiddenIdField versionFieldSet Nothing
  let formParam = MediaDelForm
                    <$> delIdRes
                    <*> versionRes
      widget = return ()
  return (formParam, widget)
