{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Frame (
    getFrameList
  , getFrameFromId
  , updateFrame
  , registerFrame
  , updateFrameValidFlag
  , deleteFrame
  , readPostFrame
  ) where

import Import
import qualified Prelude as P
import Database.Persist.Sql (BackendKey(..))
import qualified Database.Esqueleto as E
import qualified Database.Esqueleto.Internal.Sql as E
import DataTypes.HoubouType
import UrlParam.FrameId
import Libs.Common
import Service.Common

getFrameList ::
  PageInfo ->
  Handler (HResult (Int, [FrameList]))
getFrameList pageInfo = runDB $ do
  let pageNum = unPageNum pageInfo
      pagePerLine = fromIntegral $ unPagePerLine pageInfo
      countQuery = E.from $ \tblFrame -> do
        return $ E.count(tblFrame E.^. TblFrameId)
      baseQuery = E.from $ \tblFrame -> do
        E.orderBy [ E.desc $ tblFrame E.^. TblFrameValidFlag, E.desc $ tblFrame E.^. TblFrameUpdateTime ]
        let rowNum = E.unsafeSqlValue "row_number() over(order by update_time desc)"
        return
          (
            rowNum
          , tblFrame E.^. TblFrameId
          , tblFrame E.^. TblFrameName
          , tblFrame E.^. TblFrameValidFlag
          , tblFrame E.^. TblFramePublishDate
          , tblFrame E.^. TblFrameCreateTime
          , tblFrame E.^. TblFrameUpdateTime
          , tblFrame E.^. TblFrameVersion
          )
      baseQueryPage = do
        r <- baseQuery
        E.offset (pagePerLine * (intToInt64 pageNum - 1))
        E.limit pagePerLine
        return r
  list <- E.select baseQueryPage
  totalCnt <- E.select countQuery
  return $ Right (E.unValue $ totalCnt P.!! 0, toFrameList <$> list)

getFrameFromId ::
  FrameId
  -> Handler (HResult Frame)
getFrameFromId (FrameId frameId) = runDB $ do
  case frameId of
    (Just fid) -> do
      frame <- get $ toTblFrameKey fid
      case frame of
        Just f -> return $ Right $ 
                    Frame {
                        unFrameId = fid
                      , unFrameName = tblFrameName f
                      , unFrameHtml = tblFrameHtml f
                      , unFrameCss = tblFrameCss f
                      , unFrameValidFlag = tblFrameValidFlag f
                      , unFramePublishDate = tblFramePublishDate f
                      , unFrameCreateTime = tblFrameCreateTime f
                      , unFrameUpdateTime = tblFrameUpdateTime f
                      , unFrameVersion = tblFrameVersion f
                      }
        _ -> return $ Left ErrRecNotFound
    Nothing -> return $ Left ErrParam

updateFrameValidFlag ::
  Key TblFrame
  -> RecVersion
  -> Handler (HResult Int64)
updateFrameValidFlag frameKey recver = runDB $ do
  now <- liftIO getTm
  r <- get frameKey
  res <- case r of
    Nothing -> return $ Left ErrRecNotFound
    Just record -> do
      let pver = unRecVersion recver
          dver= tblFrameVersion record
      case checkVersion dver pver of
        False -> return $ Left ErrRecVersion
        True -> do
          updateWhere [
            TblFrameValidFlag ==. True
            ] [
              TblFrameValidFlag =. False
            , TblFrameUpdateTime =. now
            , TblFrameVersion +=. 1
            ]
          update frameKey $ [
              TblFrameValidFlag =. True
            , TblFrameUpdateTime =. now
            , TblFrameVersion +=. 1
            ]
          return $ Right $ fromTblFrameKey frameKey
  return res

updateFrame ::
  Frame
  -> Handler (HResult Int64)
updateFrame frame = runDB $ do
  let frameKey = toTblFrameKey $ unFrameId frame
  now <- liftIO getTm
  curRec <- get frameKey
  res <- case curRec of
    Nothing -> return $ Left ErrRecNotFound
    Just record -> do
      let pver = unFrameVersion frame
          dver = tblFrameVersion record
      case checkVersion dver pver of
        False -> return $ Left ErrRecVersion
        _ -> do
          update frameKey [
              TblFrameName =. unFrameName frame
            , TblFrameHtml =. unFrameHtml frame
            , TblFrameCss =. unFrameCss frame
            , TblFrameUpdateTime =. now
            , TblFrameVersion +=. 1
            ]
          return $ Right (unFrameId frame)
  return res

deleteFrame ::
  Key TblFrame
  -> RecVersion
  -> Handler (HResult Int64)
deleteFrame frameKey recver = runDB $ do
  curRec <- get frameKey
  res <- case curRec of
    Nothing -> return $ Left ErrRecNotFound
    Just frame -> do
      let pver = unRecVersion recver
          dver = tblFrameVersion frame
          frameId = fromTblFrameKey frameKey
      case checkVersion dver pver of
        False -> return $ Left ErrRecVersion
        True -> case tblFrameValidFlag frame of
                  True -> return $ Left ErrNotDelete
                  False -> delete frameKey >>= \_ -> return $ Right frameId
  return res

registerFrame ::
  Frame
  -> Handler (HResult Int64)
registerFrame frame = runDB $ do
  now <- liftIO getTm
  TblFrameKey (SqlBackendKey frameId) <- insert $
    TblFrame {
        tblFrameName = unFrameName frame
      , tblFrameHtml = unFrameHtml frame
      , tblFrameCss = unFrameCss frame
      , tblFrameValidFlag = unFrameValidFlag frame
      , tblFramePublishDate = Nothing
      , tblFrameCreateTime = now
      , tblFrameUpdateTime = now
      , tblFrameVersion = 1
      }
  return (Right frameId)

readPostFrame ::
  Handler (HResult Frame)
readPostFrame = runDB $ do
  f <- selectFirst [ TblFrameValidFlag ==. True ] []
  case f of
    Just frame -> return $ Right (toFrame frame)
    _ -> return $ Left ErrRecNotFound
