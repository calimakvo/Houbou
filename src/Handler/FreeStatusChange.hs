{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.FreeStatusChange where

import Import
import DataTypes.HoubouType
import Libs.Common
import Libs.CommonWidget
import Forms.StatusChgFreeForm
import Service.Common
import Service.Free

postFreeStatusChangeR ::
  Handler TypedContent
postFreeStatusChangeR = do
  ((res, _), _) <- runFormPost statusChgFreeForm
  case res of
    FormSuccess (StatusChgFreeForm freeId version) -> do
      result <- updateFreeStatus (toTblFreeKey freeId) (RecVersion version)
      case result of
        Right (fs, newRecVer) -> retJson (fromEnum fs) newRecVer ""
        Left (err, curRecVer)  -> do
          $(logError) $ "postFreeStatusChangeR: Free Status update failure error=" <> (toText err)
          let msg = parmErrToMsg err
          retJson (fromEnum UnkStatus) curRecVer msg
    _ -> retJson (fromEnum UnkStatus) 0 ""
