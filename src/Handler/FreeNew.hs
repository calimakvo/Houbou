{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.FreeNew where

import Import
import DataTypes.HoubouType
import Libs.Common
import Libs.CommonWidget
import UrlParam.FreeId
import Forms.FreeForm
import Libs.Mapper
import Service.Free
import Service.Tags
import Service.Category

getFreeNewR ::
  Handler Html
getFreeNewR = do
  msg <- getMessages
  let tokenKey = defaultCsrfParamName
  token <- getRequest >>= createCsrfToken
  cateMap <- getCategoryRel
  (freeWidget, _) <- generateFormPost $ freeForm Nothing cateMap
  defaultLayout $ do
    setTitle title
    $(widgetFile "freenew")

postFreeNewR ::
  Handler Html
postFreeNewR = do
  msg <- getMessages
  let tokenKey = defaultCsrfParamName
  token <- getRequest >>= createCsrfToken
  (usrKey, _) <- requireAuthPair
  cateMap <- getCategoryRel
  ((res, freeWidget), _) <- runFormPost $ freeForm Nothing cateMap
  case res of
    FormSuccess form -> do
      let free = freeFormToFree form
      result <- registerFree usrKey free
      case result of
        Right fid -> do
          _ <- case unFreeTags free of
                 Just tags -> do
                   _ <- updateFreeTagList fid tags
                   return ()
                 Nothing -> return ()
          addMessage successKey "登録完了しました"
          redirect $ FreeR (FreeId $ Just fid)
        Left err -> do
          $(logError) $ "postFreeNewR: free register failure err=" <> toText err
          addMessage errorKey "登録失敗しました"
          defaultLayout $ do
            setTitle title
            $(widgetFile "freenew")
    _ -> do
      defaultLayout $ do
        setTitle title
        $(widgetFile "freenew")

title :: Html
title = "フリーページ新規登録"
