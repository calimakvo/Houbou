{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.PostList where

import Import
import UrlParam.Page
import UrlParam.PostId
import DataTypes.HoubouType
import Forms.ListSelectForm
import Libs.Common
import Libs.CommonWidget
import Service.Common
import Service.Post

getPostListR ::
  Int
  -> Page
  -> Handler Html
getPostListR pageType page = do
  master <- getYesod
  msg <- getMessages
  token <- getRequest >>= createCsrfTokenTag
  let defLine = defSelectorValue master
  pagePerLine <- textToInt <$> (fromMaybe defLine) <$> (lookupSession pageparline)
  let pageNum = fromMaybe 1 (_unPage page)
      pageInfo = PageInfo pageNum pagePerLine
      (postViewStatus, headerStr) = searchType pageType
      truePageType = fromEnum postViewStatus
  setSession listTypeKey (pack(show(truePageType)))
  (pagePerLineWidget, _) <- generateFormPost $ listSelectForm (Just pagePerLine)
  (totalCnt, posts) <- eitherToTouple =<< getPostList postViewStatus pageInfo
  defaultLayout $ do
    let pager = paginator pageNum totalCnt pagePerLine
        postList = zip ([0..] :: [Int]) posts
    setTitle title
    $(widgetFile "postlist")

postPostListR ::
  Int
  -> Page
  -> Handler Html
postPostListR pageType _ = do
  ((res, _), _) <- runFormPost $ listSelectForm Nothing
  case res of
    FormSuccess (ListSelectForm pagePerLine) -> do
      setSession pageparline $ pack(show(pagePerLine))
    _ -> return ()
  redirect $ PostListR pageType (Page Nothing)

title :: Html
title = "投稿一覧"
