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
import Libs.Template
import Forms.PrevForm
import Service.Html
import Service.Preview

getFreePrevR :: Handler Html
getFreePrevR = do
  master <- getYesod
  form <- lookupSession prevfreeform >>= chkForm
  let prev = toPrevForm form
      defFree = prevFormToFree prev
      contents = maybeToText $ unPrevFormPreviewContent prev
      css =  maybeToText $ unPrevFormPreviewCss prev
      setId = appBlogSettingId $ appSettings master
  chk <- liftIO $ checkTemplate (unpack contents)
  chkcss <- liftIO $ checkTemplate (unpack css)
  case chk of
    Nothing ->
      case chkcss of
        Nothing -> do
          free <- createPrevFree defFree
          res <- createPrevBlogFreeContents setId (FreeInf free [])
          case res of
            Right html -> return html
            Left _ -> return "プレビューできません"
        Just err -> return $ toHtml (errPrevText err)
    Just err -> return $ toHtml (errPrevText err)

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
