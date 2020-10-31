{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.FreePrev where

import Import
import DataTypes.HoubouType
import Libs.Common
import Libs.CommonWidget
import Libs.Mapper
import Forms.PrevForm
import Service.Html
import Service.Preview

getFreePrevR :: Handler Html
getFreePrevR = do
  master <- getYesod
  form <- lookupSession prevfreeform >>= chkForm
  let prev = toPrevForm form
      defFree = prevFormToFree prev
      setId = appBlogSettingId $ appSettings master
  free <- createPrevFree defFree
  res <- createPrevBlogFreeContents setId free
  case res of
    Right html -> return html
    Left _ -> return "プレビューできません"

postFreePrevR ::
  Handler TypedContent
postFreePrevR = do
  ((res, _), _) <- runFormPost prevForm
  case res of
    FormSuccess prev -> do
      setSession prevfreeform $ encodePrevFormParm prev
      retJson (fromEnum Success) 0 ""
    _ -> retJson (fromEnum Failure) 0 "プレビューエラー"

title :: Html
title = "プレビュー"

