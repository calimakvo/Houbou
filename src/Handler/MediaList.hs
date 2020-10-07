{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.MediaList where

import Import
import DataTypes.HoubouType
import UrlParam.Page
import UrlParam.MediaId
import Forms.ListSelectForm
import Libs.Common
import Libs.CommonWidget
import Service.Common
import Service.Media
import Service.BlogSetting

getMediaListR ::
  Page
  -> Handler Html
getMediaListR page = do
  master <- getYesod
  msg <- getMessages
  token <- getRequest >>= createCsrfTokenTag
  let defLine = defSelectorValue master
      setId = appBlogSettingId $ appSettings master
  pagePerLine <- textToInt <$> (fromMaybe defLine) <$> (lookupSession pageparline)
  setting <- getBlogSetting setId
  let pageNum = fromMaybe 1 (_unPage page)
      pageInfo = PageInfo pageNum pagePerLine
  (pagePerLineWidget, _) <- generateFormPost $ listSelectForm (Just pagePerLine)
  (totalCnt, medias) <- eitherToTouple =<< getMediaList pageInfo
  defaultLayout $ do
    let pager = paginator pageNum totalCnt pagePerLine
        mediaList = zip ([0..] :: [Int]) medias
    setTitle title
    $(widgetFile "medialist")
  
postMediaListR ::
  Page
  -> Handler Html
postMediaListR _ = do
  ((res, _), _) <- runFormPost $ listSelectForm Nothing
  case res of
    FormSuccess (ListSelectForm pagePerLine) -> do
      setSession pageparline $ pack(show(pagePerLine))
    _ -> return ()
  redirect $ MediaListR (Page Nothing)

title :: Html
title = "メディア"
