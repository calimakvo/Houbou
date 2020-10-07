{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.FreeFrameList where

import Import
import DataTypes.HoubouType
import UrlParam.Page
import Libs.Common
import Libs.CommonWidget
import Forms.ListSelectForm
import UrlParam.FreeFrameId
import Service.Common
import Service.FreeFrame

getFreeFrameListR ::
  Page
  -> Handler Html
getFreeFrameListR page = do
  master <- getYesod
  msg <- getMessages
  token <- getRequest >>= createCsrfTokenTag
  let defLine = defSelectorValue master
  pagePerLine <- textToInt <$> (fromMaybe defLine) <$> (lookupSession freepageperline)
  let pageNum = fromMaybe 1 (_unPage page)
      pageInfo = PageInfo pageNum pagePerLine
  (pagePerLineWidget, _) <- generateFormPost $ listSelectForm (Just pagePerLine)
  (totalCnt, frames) <- eitherToTouple =<< getFreeFrameList pageInfo
  defaultLayout $ do
    let pager = paginator pageNum totalCnt pagePerLine
    setTitle title
    $(widgetFile "freeframelist")

postFreeFrameListR ::
  Page
  -> Handler Html
postFreeFrameListR _ = do
  ((res, _), _) <- runFormPost $ listSelectForm Nothing
  case res of
    FormSuccess (ListSelectForm pagePerLine) -> do
      setSession freepageperline $ pack(show(pagePerLine))
    _ -> return ()
  redirect $ FreeFrameListR (Page Nothing)

title :: Html
title = "フリーフレーム"
