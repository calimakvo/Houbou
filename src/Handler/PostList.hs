{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.PostList where

import Import
import UrlParam.PostId
import DataTypes.HoubouType
import Forms.ListSearchSelectForm
import Forms.SearchForm
import Libs.Mapper
import Libs.Common
import Libs.CommonWidget
import Service.Common
import Service.Post
import Service.Category

getPostListR ::
  Handler Html
getPostListR = do
  master <- getYesod
  msg <- getMessages
  token <- getRequest >>= createCsrfTokenTag
  cateMap <- getCategoryRel
  let defLine = defSelectorValue master
  pagePerLine <- textToInt <$> (fromMaybe defLine) <$> (lookupSession pageparline)
  sparam <- getListSearchParam pagePerLine
  let pageNum = unSearchParamPage sparam
      pageInfo = PageInfo pageNum pagePerLine
      (_, headerStr) = searchType (fromEnum $ unSearchParamSearchType sparam)
  (postPerLineWidget, _) <- generateFormPost $ listSearchSelectForm (Just sparam)
  (searchWidget, _) <- generateFormPost $ searchForm (Just sparam) cateMap
  (totalCnt, posts) <- eitherToTouple =<< getPostList sparam pageInfo
  defaultLayout $ do
    let pager = paginator pageNum totalCnt pagePerLine
        postList = zip ([0..] :: [Int]) posts
    setTitle title
    $(widgetFile "postlist")

postPostListR ::
  Handler Html
postPostListR = do
  master <- getYesod
  let defLine = defSelectorValue master
  pagePerLine <- textToInt <$> (fromMaybe defLine) <$> (lookupSession freepageperline)
  ((res, _), _) <- runFormPost $ listSearchSelectForm Nothing
  case res of
    FormSuccess form -> do
      setSession pageparline $ pack(show(unListSearchSelectFormPagePerLine form))
      let sparam = listSearchSelectFormToSearchParam form pagePerLine
      redirect (PostListR, toListParam sparam)
    _ -> redirect PostListR

title :: Html
title = "投稿一覧"
