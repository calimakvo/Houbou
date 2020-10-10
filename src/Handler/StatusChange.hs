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
        Right (ps, newRecVer) -> retJson (fromEnum ps) newRecVer ""
        Left (err, curRecVer) -> do
          $(logError) $ "postStatusChangeR: Post status update failure error = " <> (toText err)
          let msg = parmErrToMsg err
          retJson (fromEnum UnkStatus) curRecVer msg
    _ -> retJson (fromEnum UnkStatus) 0 ""
