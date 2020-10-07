{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.FrameList where

import Import
import UrlParam.Page
import UrlParam.FrameId
import DataTypes.HoubouType
import Forms.ListSelectForm
import Libs.Common
import Libs.CommonWidget
import Service.Common
import Service.Frame

getFrameListR ::
  Page
  -> Handler Html
getFrameListR page = do
  master <- getYesod
  msg <- getMessages
  token <- getRequest >>= createCsrfTokenTag
  let defLine = defSelectorValue master
  pagePerLine <- textToInt <$> (fromMaybe defLine) <$> (lookupSession framepageperline)
  let pageNum = fromMaybe 1 (_unPage page)
      pageInfo = PageInfo pageNum pagePerLine
  (pagePerLineWidget, _) <- generateFormPost $ listSelectForm (Just pagePerLine)
  (totalCnt, frames) <- eitherToTouple =<< getFrameList pageInfo
  defaultLayout $ do
    let pager = paginator pageNum totalCnt pagePerLine
    setTitle title
    $(widgetFile "framelist")

postFrameListR ::
  Page
  -> Handler Html
postFrameListR _ = do
  ((res, _), _) <- runFormPost $ listSelectForm Nothing
  case res of
    FormSuccess (ListSelectForm pagePerLine) -> do
      setSession framepageperline $ pack(show(pagePerLine))
    _ -> return ()
  redirect $ FrameListR (Page Nothing)

title :: Html
title = "フレーム"
