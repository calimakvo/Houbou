{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.FreeDelForm (
    freeDelForm
  , FreeDelForm(..)
  ) where

import Import
import Forms.FormValid
import Forms.CommonForm

data FreeDelForm = FreeDelForm {
    unFreeDelFormId :: Int64
  , unFreeDelFormVersion :: Int
}

freeDelForm :: Html -> MForm Handler (FormResult FreeDelForm, Widget)
freeDelForm _ = do
  (delIdRes, _) <- mreq hiddenId64Field freeIdFieldSet Nothing
  (versionRes, _) <- mreq hiddenIdField versionFieldSet Nothing
  let formParam = FreeDelForm
                    <$> delIdRes
                    <*> versionRes
      widget = return ()
  return (formParam, widget)
