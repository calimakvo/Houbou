{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.FreeFrameChgForm (
    freeFrameChgForm
  , FreeFrameChgForm(..)
  ) where

import Import
import Forms.FormValid
import Forms.CommonForm

data FreeFrameChgForm = FreeFrameChgForm {
    unFreeFrameChgFormFrmId :: Int64
  , unFreeFrameChgFormVersion :: Int
}

freeFrameChgForm :: Html -> MForm Handler (FormResult FreeFrameChgForm, Widget)
freeFrameChgForm _ = do
  (frameIdRes, _) <- mreq hiddenId64Field freeFrameIdFieldSet Nothing
  (versionRes, _) <- mreq hiddenIdField versionFieldSet Nothing
  let formParam = FreeFrameChgForm
                    <$> frameIdRes
                    <*> versionRes
      widget = return ()
  return (formParam, widget)
