{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.FrameDelForm (
    frameDelForm
  , FrameDelForm(..)
  ) where

import Import
import Forms.FormValid
import Forms.CommonForm

data FrameDelForm = FrameDelForm {
    unFrameDelFormId :: Int64
  , unFrameDelFormVersion :: Int
}

frameDelForm :: Html -> MForm Handler (FormResult FrameDelForm, Widget)
frameDelForm _ = do
  (delIdRes, _) <- mreq hiddenId64Field frameIdFieldSet Nothing
  (versionRes, _) <- mreq hiddenIdField versionFieldSet Nothing
  let formParam = FrameDelForm
                    <$> delIdRes
                    <*> versionRes
      widget = return ()
  return (formParam, widget)
