{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.FrameNew where

import Import
import Libs.Common
import UrlParam.FrameId
import Forms.FrameForm
import Libs.Mapper
import Service.Frame

getFrameNewR ::
  Handler Html
getFrameNewR = do
  msg <- getMessages
  (frameWidget, _) <- generateFormPost $ frameForm Nothing
  defaultLayout $ do
    setTitle title
    $(widgetFile "frame")

postFrameNewR ::
  Handler Html
postFrameNewR = do
  msg <- getMessages
  ((res, frameWidget), _) <- runFormPost $ frameForm Nothing
  case res of
    FormSuccess form -> do
      result <- registerFrame (frameFormToFrame form)
      case result  of
        Right frameId -> do
          addMessage successKey "登録完了しました"
          redirect $ FrameR (FrameId $ Just frameId)
        Left err -> do
          $(logError) $ "Frame resgiter failed: " <> (toText err)
          addMessage errorKey "登録失敗しました"
          defaultLayout $ do
            setTitle title
            $(widgetFile "frame")
    _ -> do
      defaultLayout $ do
        setTitle title
        $(widgetFile "frame")

title :: Html
title = "フレーム新規登録"
