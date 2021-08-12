{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.AccDay where

import Import
import Data.Time
import DataTypes.HoubouType
import UrlParam.PostId
import UrlParam.FreeId
import Libs.Common
import Libs.CommonWidget
import Service.Access

getAccDayR :: Handler Html
getAccDayR = do
  msg <- getMessages
  (f, t) <- getDateFromToParam
  let from = dateTextToUTimeSlush f
      to = addUTCTime (60*60*1*24-1) <$> dateTextToUTimeSlush t
      empmsg = if isNothing from == True && isNothing to == True then
                  Just "日付を指定してください" :: Maybe Text
               else
                  Nothing
  accList <- case empmsg of
           Just _ -> return []
           Nothing -> getAccessDay from to
  defaultLayout $ do
    setTitle title
    $(widgetFile "accday")

title :: Html
title = "日別ページアクセス"
