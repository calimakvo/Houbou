{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.FreeList where

import Import
import UrlParam.FreeId
import UrlParam.CateId
import DataTypes.HoubouType
import Forms.ListSearchSelectForm
import Forms.SearchForm
import Libs.Mapper
import Libs.Common
import Libs.CommonWidget
import Service.Common
import Service.Free
import Service.Category

getFreeListR ::
  Handler Html
getFreeListR = do
  master <- getYesod
  msg <- getMessages
  token <- getRequest >>= createCsrfTokenTag
  cateMap <- getCategoryRel
  let defLine = defSelectorValue master
  pagePerLine <- textToInt <$> (fromMaybe defLine) <$> (lookupSession freepageperline)
  sparam <- getListSearchParam pagePerLine
  let pageNum = unSearchParamPage sparam
      pageInfo = PageInfo pageNum pagePerLine
  (freePerLineWidget, _) <- generateFormPost $ listSearchSelectForm (Just sparam)
  (searchWidget, _) <- generateFormPost $ searchForm (Just sparam) cateMap
  (totalCnt, frees) <- eitherToTouple =<< getFreeList sparam pageInfo
  defaultLayout $ do
    let pager = paginator pageNum totalCnt pagePerLine
        freeList = zip ([0..] :: [Int]) frees
    setTitle title
    $(widgetFile "freelist")

postFreeListR ::
  Handler Html
postFreeListR = do
  master <- getYesod
  let defLine = defSelectorValue master
  pagePerLine <- textToInt <$> (fromMaybe defLine) <$> (lookupSession freepageperline)
  ((res, _), _) <- runFormPost $ listSearchSelectForm Nothing
  case res of
    FormSuccess form -> do
      setSession freepageperline $ pack(show(unListSearchSelectFormPagePerLine form))
      let sparam = listSearchSelectFormToSearchParam form pagePerLine
      redirect (FreeListR, toListParam sparam)
    _ -> redirect FreeListR

title :: Html
title = "フリーページ"
