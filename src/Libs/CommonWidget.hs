{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Libs.CommonWidget (
    createCsrfTokenTag
  , createCsrfToken
  , defSelectorValue
  , urlPost
  , urlPostSlug
  , urlFree
  , urlFreeSlug
  , retJson
  , retPlain
  , genError
  , toPostSlugUrlText
  , toPostUrlText
  , toFreeSlugUrlText
  , toFreeUrlText
  , toTagListUrlText
  , canonicalPath
  , chkForm
  ) where

import Import
import qualified Prelude as P
import DataTypes.HoubouType
import UrlParam.YPath
import UrlParam.MPath
import UrlParam.DPath
import UrlParam.Slug
import UrlParam.PostId
import UrlParam.FreeId
import UrlParam.TagId
import qualified Data.Text as T

createCsrfToken ::
  YesodRequest
  -> Handler T.Text
createCsrfToken req = do
  case reqToken req of
    Nothing -> return $ mempty
    Just n -> return n
  
createCsrfTokenTag ::
  YesodRequest
  -> Handler Widget
createCsrfTokenTag req = do
  let tokenKey = defaultCsrfParamName
  case reqToken req of
    Nothing -> return $ mempty
    Just n -> return $ toWidget [hamlet|
                   <input type=hidden name=#{tokenKey} value=#{n}>
              |]

defSelectorValue ::
  App
  -> T.Text
defSelectorValue master = pack(show(xs P.!! 0))
  where
    xs = appPagePerLineSelector $ appSettings master

retPlain ::
  String -> Handler TypedContent
retPlain val = selectRep $ do
  provideRep $ return $ repPlain val

retJson ::
  Int
  -> Int
  -> T.Text
  -> Handler TypedContent
retJson result version msg = selectRep $ do
  provideRep $ return $ repJson (
    object [
          "result" .= result
        , "msg" .= msg
        , "version" .= version
        ])

genError ::
  Text
  -> Handler Html
genError err_message = defaultLayout $ do
  $(widgetFile "generror")

urlPost ::
  Int64
  -> Route App
urlPost pid = PutR (PostId $ Just pid)

urlFree ::
  Int64
  -> Route App
urlFree fid = PutFreeR (FreeId $ Just fid)

urlPostSlug ::
  Maybe Text
  -> Maybe Text
  -> Route App
urlPostSlug slug urlpath =
  let (y, m, d) = parseUrlPath urlpath
  in PutSlugR
       (YPath $ Just y)
       (MPath $ Just m)
       (DPath $ Just d)
       (Slug $ slug)

urlFreeSlug ::
  Maybe Text
  -> Maybe Text
  -> Route App
urlFreeSlug slug urlpath =
  let (y, m, d) = parseUrlPath urlpath
  in PutFreeSlugR
       (YPath $ Just y)
       (MPath $ Just m)
       (DPath $ Just d)
       (Slug $ slug)

parseUrlPath ::
  Maybe Text
  -> (T.Text, T.Text, T.Text)
parseUrlPath urlpath =
  case urlpath of
    Nothing -> error "parseUrlPath: Slug URL error param is Nothing"
    Just p ->
      let xs = T.splitOn (T.pack "/") p
      in
        if length xs == 4
        then
          (xs P.!! 0, xs P.!! 1, xs P.!! 2)
        else
          error "parseUrlPath: Slug URL parse error"

urlTagList ::
  Int64
  -> Route App
urlTagList = PutTagListR . TagId . Just

toTagListUrlText ::
  Int64
  -> T.Text
toTagListUrlText tagId =
  let (urls, _) = renderRoute $ urlTagList tagId
  in T.intercalate "/" urls

toPostSlugUrlText ::
  Maybe T.Text
  -> Maybe T.Text
  -> T.Text
toPostSlugUrlText slug urlpath =
  let (urls, _) = renderRoute $ urlPostSlug slug urlpath
  in T.intercalate "/" urls

toPostUrlText ::
  Int64
  -> T.Text
toPostUrlText pid =
  let (urls, _) = renderRoute $ urlPost pid
  in T.intercalate "/" urls

toFreeSlugUrlText ::
  Maybe T.Text
  -> Maybe T.Text
  -> T.Text
toFreeSlugUrlText slug urlpath =
  let (urls, _) = renderRoute $ urlFreeSlug slug urlpath
  in T.intercalate "/" urls

toFreeUrlText ::
  Int64
  -> T.Text
toFreeUrlText fid =
  let (urls, _) = renderRoute $ urlFree fid
  in T.intercalate "/" urls

canonicalPath ::
  PageCanonicalType
  -> Maybe Text
  -> Maybe Text
  -> Text
canonicalPath ctype urlpath slug =
  if P.all isJust [urlpath, slug] == False
  then ""
  else
    case ctype of
      CPost -> toPostSlugUrlText slug urlpath
      CFree -> toFreeSlugUrlText slug urlpath

chkForm :: (Maybe Text) -> Handler Text
chkForm = \form ->
  case form of
    Just prev -> return prev
    Nothing -> error "Preview: param error"
