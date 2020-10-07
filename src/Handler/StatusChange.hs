{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.StatusChange where

import Import
import DataTypes.HoubouType
import Forms.StatusChgForm
import Libs.Common
import Libs.CommonWidget
import Service.Common
import Service.Post

postStatusChangeR ::
  Handler TypedContent
postStatusChangeR = do
  ((res, _), _) <- runFormPost statusChgForm
  case res of
    FormSuccess (StatusChgForm postId version) -> do
      result <- updatePostStatus (toTblPostKey postId) (RecVersion version)
      case result of
        Right ps -> retJson (fromEnum ps) version
        Left err -> do
          $(logError) $ "postStatusChangeR: error = " <> (toText err)
          retJson (fromEnum UnkStatus) 0
    _ -> retJson (fromEnum UnkStatus) 0
