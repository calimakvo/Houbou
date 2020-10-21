{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.MediaIns where

import Import
import UrlParam.Page
import DataTypes.HoubouType
import Forms.ListSelectForm
import Libs.Common
import Libs.CommonWidget
import Service.Common
import Service.Media
import Service.BlogSetting

getMediaInsR ::
  Page
  -> Handler Html
getMediaInsR page = do
  master <- getYesod
  msg <- getMessages
  let defLine = defSelectorValue master
      setId = appBlogSettingId $ appSettings master
  pagePerLine <- textToInt <$> (fromMaybe defLine) <$> (lookupSession pageparline)
  setting <- getBlogSetting setId
  let pageNum = fromMaybe 1 (_unPage page)
      pageInfo = PageInfo pageNum pagePerLine
  (pagePerLineWidget, _) <- generateFormPost $ listSelectForm (Just pagePerLine)
  (totalCnt, medias) <- eitherToTouple =<< getMediaList pageInfo
  genericLayout $ do
    let pager = paginator pageNum totalCnt pagePerLine
        mediaList = zip ([0..] :: [Int]) medias
    setTitle title
    $(widgetFile "mediains")

postMediaInsR ::
  Page
  -> Handler Html
postMediaInsR _ = do
  ((res, _), _) <- runFormPost $ listSelectForm Nothing
  case res of
    FormSuccess (ListSelectForm pagePerLine) -> do
      setSession pageparline $ pack(show(pagePerLine))
    _ -> return ()
  redirect $ MediaInsR (Page Nothing)

title :: Html
title = "メディア挿入"
