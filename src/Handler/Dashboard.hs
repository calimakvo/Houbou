{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Dashboard where

import Import
import Libs.Common
import Service.Common
import Service.Access

getDashboardR ::
  Handler Html
getDashboardR = do
  msg <- getMessages
  cnt1 <- eitherToList =<< getTatolTopAccess
  cnt2 <- eitherToList =<< getTatolAccess
  defaultLayout $ do
    let bars = createBar1Data cnt1
        bars2 = createBar2Data cnt2
    setTitle title
    $(widgetFile "dashboard")

title :: Html
title = "ダッシュボード"

-- 
