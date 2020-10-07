{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.FreeFrameNew where

import Import
import Libs.Common
import UrlParam.FreeFrameId
import Libs.Mapper
import Forms.FreeFrameForm
import Service.FreeFrame

getFreeFrameNewR ::
  Handler Html
getFreeFrameNewR = do
  msg <- getMessages
  (freeFrameWidget, _) <- generateFormPost $ (freeFrameForm Nothing)
  defaultLayout $ do
    setTitle title
    $(widgetFile "freeframe")

postFreeFrameNewR ::
  Handler Html
postFreeFrameNewR = do
  msg <- getMessages
  ((res, freeFrameWidget), _) <- runFormPost $ freeFrameForm Nothing
  case res of
    FormSuccess form -> do
      result <- registerFreeFrame (freeFrameFormToFreeFrame form)
      case result  of
        Right freeFrameId -> do
          addMessage successKey "登録完了しました"
          redirect $ FreeFrameR (FreeFrameId $ Just freeFrameId)
        Left err -> do
          $(logError) $ "Free frame resgiter failed: " <> (toText err)
          addMessage errorKey "登録失敗しました"
          defaultLayout $ do
            setTitle title
            $(widgetFile "freeframe")
    _ -> do
      defaultLayout $ do
        setTitle title
        $(widgetFile "freeframe")

title :: Html
title = "フリーページフレーム新規登録"
