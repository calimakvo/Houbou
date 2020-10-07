{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.FreeFrameDelForm (
    freeFrameDelForm
  , FreeFrameDelForm(..)
  ) where

import Import
import Forms.FormValid
import Forms.CommonForm

data FreeFrameDelForm = FreeFrameDelForm {
    unFreeFrameDelFormId :: Int64
  , unFreeFrameDelFormVersion :: Int
}

freeFrameDelForm :: Html -> MForm Handler (FormResult FreeFrameDelForm, Widget)
freeFrameDelForm _ = do
  (delIdRes, _) <- mreq hiddenId64Field freeFrameIdFieldSet Nothing
  (versionRes, _) <- mreq hiddenIdField versionFieldSet Nothing
  let formParam = FreeFrameDelForm
                    <$> delIdRes
                    <*> versionRes
      widget = return ()
  return (formParam, widget)
