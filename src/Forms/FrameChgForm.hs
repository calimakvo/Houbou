{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.FrameChgForm (
    frameChgForm
  , FrameChgForm(..)
  ) where

import Import
import Forms.FormValid
import Forms.CommonForm

data FrameChgForm = FrameChgForm {
    unFrmChgFormFrmId :: Int64
  , unFrmChgFormVersion :: Int
}

frameChgForm :: Html -> MForm Handler (FormResult FrameChgForm, Widget)
frameChgForm _ = do
  (frameIdRes, _) <- mreq hiddenId64Field frameIdFieldSet Nothing
  (versionRes, _) <- mreq hiddenIdField versionFieldSet Nothing
  let formParam = FrameChgForm
                    <$> frameIdRes
                    <*> versionRes
      widget = return ()
  return (formParam, widget)
