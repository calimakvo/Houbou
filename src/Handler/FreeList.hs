{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.FreeList where

import Import
import UrlParam.Page
import UrlParam.FreeId
import DataTypes.HoubouType
import Forms.ListSelectForm
import Libs.Common
import Libs.CommonWidget
import Service.Common
import Service.Free

getFreeListR ::
  Int
  -> Page
  -> Handler Html
getFreeListR pageType page =  do
  master <- getYesod
  msg <- getMessages
  token <- getRequest >>= createCsrfTokenTag
  let defLine = defSelectorValue master
      (freeViewStatus, _) = searchType pageType
      truePageType = fromEnum freeViewStatus
  pagePerLine <- textToInt <$> (fromMaybe defLine) <$> (lookupSession freepageperline)
  let pageNum = fromMaybe 1 (_unPage page)
      pageInfo = PageInfo pageNum pagePerLine
  (freePerLineWidget, _) <- generateFormPost $ listSelectForm (Just pagePerLine)
  (totalCnt, frees) <- eitherToTouple =<< getFreeList freeViewStatus pageInfo
  defaultLayout $ do
    let pager = paginator pageNum totalCnt pagePerLine
        freeList = zip ([0..] :: [Int]) frees
    setTitle title
    $(widgetFile "freelist")

postFreeListR ::
  Int
  -> Page
  -> Handler Html
postFreeListR pageType _ = do
  ((res, _), _) <- runFormPost $ listSelectForm Nothing
  case res of
    FormSuccess (ListSelectForm pagePerLine) -> do
      setSession freepageperline $ pack(show(pagePerLine))
    _ -> return ()
  redirect $ FreeListR pageType (Page Nothing)

title :: Html
title = "フリーページ"
