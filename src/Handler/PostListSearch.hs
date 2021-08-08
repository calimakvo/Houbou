{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.PostListSearch where

import Import
import Libs.Common
import Libs.CommonWidget
import Libs.Mapper
import Forms.SearchForm
import Service.Category

postPostListSearchR :: Handler Html
postPostListSearchR = do
  master <- getYesod
  let defLine = defSelectorValue master
  pagePerLine <- textToInt <$> (fromMaybe defLine) <$> (lookupSession freepageperline)
  cateMap <- getCategoryRel
  ((res, _), _) <- runFormPost $ searchForm Nothing cateMap
  case res of
    FormSuccess form -> do
      let sparam = searchFromToSearchParam form pagePerLine
      redirect (PostListR, toListParam sparam)
    _ -> redirect PostListR
