{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.FreeListSearch where

import Import
import Libs.Common
import Libs.CommonWidget
import Libs.Mapper
import Forms.SearchForm
import Service.Category

postFreeListSearchR :: Handler Html
postFreeListSearchR = do
  master <- getYesod
  let defLine = defSelectorValue master
  pagePerLine <- textToInt <$> (fromMaybe defLine) <$> (lookupSession freepageperline)
  cateMap <- getCategoryRel
  ((res, _), _) <- runFormPost $ searchForm Nothing cateMap
  case res of
    FormSuccess form -> do
      let sparam = searchFromToSearchParam form pagePerLine
      redirect (FreeListR, toListParam sparam)
    _ -> redirect FreeListR
