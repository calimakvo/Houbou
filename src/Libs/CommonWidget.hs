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
  , retJson
  , retPlain
  , genError
  ) where

import Import
import qualified Prelude as P

createCsrfToken ::
  YesodRequest
  -> Handler Text
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
  -> Text
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
  -> Handler TypedContent
retJson result version = selectRep $ do
  provideRep $ return $ repJson (
    object [
          "result" .= result
        , "version" .= (version + 1)
        ])

genError ::
  Text
  -> Handler Html
genError err_message = defaultLayout $ do
  $(widgetFile "generror")
