{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Frame where

import Import
import UrlParam.FrameId
import Libs.Common
import Libs.Mapper
import Forms.FrameForm
import Service.Frame

getFrameR ::
  FrameId
  -> Handler Html
getFrameR frameId = do
  msg <- getMessages
  result <- getFrameFromId frameId
  case result of
    Left err -> do
      $(logError) $ "getFrameR: record not found." <> toText err
      notFound
    Right frame -> do
      (frameWidget, _) <- generateFormPost (frameForm $ Just frame)
      defaultLayout $ do
        setTitle title
        $(widgetFile "frame")

postFrameR ::
  FrameId
  -> Handler Html
postFrameR frameId = do
  msg <- getMessages
  ((res, frameWidget), _) <- runFormPost $ frameForm Nothing
  case res of
   FormSuccess form -> do
     result <- updateFrame (frameFormToFrame form)
     let frameId' = unFrameFormId form
     case result of
       Right _ -> addMessage successKey "更新完了しました"
       Left err -> do
         $(logError) $ "postFrameR: Frame update failure err/frameId="
           <> toText err <> "/" <> toText frameId'
         addMessage errorKey "更新失敗しました"
     redirect $ FrameR frameId
   _ -> defaultLayout $ do
     setTitle title
     $(widgetFile "frame")

title :: Html
title = "フレーム設定"
