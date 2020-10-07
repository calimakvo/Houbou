{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.PostDelForm (
    postDelForm
  , PostDelForm(..)
  ) where

import Import
import Forms.FormValid
import Forms.CommonForm

data PostDelForm = PostDelForm {
    unPostDelFormId :: Int64
  , unPostDelFormVersion :: Int
}

postDelForm :: Html -> MForm Handler (FormResult PostDelForm, Widget)
postDelForm _ = do
  (delIdRes, _) <- mreq hiddenId64Field postIdFieldSet Nothing
  (versionRes, _) <- mreq hiddenIdField versionFieldSet Nothing
  let formParam = PostDelForm
                    <$> delIdRes
                    <*> versionRes
      widget = return ()
  return (formParam, widget)
