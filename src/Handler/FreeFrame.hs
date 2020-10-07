{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.FreeFrame where

import Import
import Libs.Common
import UrlParam.FreeFrameId
import Forms.FreeFrameForm
import Libs.Mapper
import Service.FreeFrame

getFreeFrameR ::
  FreeFrameId
  -> Handler Html
getFreeFrameR freeFrameId = do
  msg <- getMessages
  result <- getFreeFrameFromId freeFrameId
  case result of
    Left err -> do
      $(logError) $ "getFreeFrameR: FreeFrame record not found err/freeFrameId="
        <> toText err <> "/" <> toText (_unUrlFreeFrameId freeFrameId) 
      notFound
    Right freeFrame -> do
      (freeFrameWidget, _) <- generateFormPost $ (freeFrameForm $ Just freeFrame)
      defaultLayout $ do
        setTitle title
        $(widgetFile "freeframe")

postFreeFrameR ::
  FreeFrameId
  -> Handler Html
postFreeFrameR freeFrameId = do
  msg <- getMessages
  ((res, freeFrameWidget), _) <- runFormPost $ freeFrameForm Nothing
  case res of
   FormSuccess form -> do
     result <- updateFreeFrame (freeFrameFormToFreeFrame form)
     case result of
       Right _ -> addMessage successKey "更新完了しました"
       Left err -> do
         $(logInfo) $ "postFreeFrameR: freeFrame update failure err/freeFrameId"
           <> (toText err) <> "/" <> toText (unFreeFrameFormId form)
         addMessage errorKey "更新失敗しました"
     redirect $ FreeFrameR freeFrameId
   _ -> defaultLayout $ do
     setTitle title
     $(widgetFile "freeframe")

title :: Html
title = "フリーフレーム"
