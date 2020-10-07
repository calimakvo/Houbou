{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.FreeFrame (
    getFreeFrameList
  , updateFreeFrame
  , getFreeFrameFromId
  , registerFreeFrame
  , updateFreeFrameValidFlag
  , deleteFreeFrame
  , readFreeFrame
  ) where

import Import
import qualified Prelude as P
import Database.Persist.Sql (BackendKey(..))
import qualified Database.Esqueleto as E
import qualified Database.Esqueleto.Internal.Sql as E
import DataTypes.HoubouType
import Libs.Common
import UrlParam.FreeFrameId
import Service.Common

getFreeFrameList ::
  PageInfo ->
  Handler (HResult (Int, [FreeFrameList]))
getFreeFrameList pageInfo = runDB $ do
  let pageNum = unPageNum pageInfo
      pagePerLine = fromIntegral $ unPagePerLine pageInfo
      countQuery = E.from $ \tblFreeFrame -> do
        return $ E.count(tblFreeFrame E.^. TblFreeFrameId)
      baseQuery = E.from $ \tblFreeFrame -> do
        E.orderBy [ E.desc $ tblFreeFrame E.^. TblFreeFrameValidFlag, E.desc $ tblFreeFrame E.^. TblFreeFrameUpdateTime ]
        let rowNum = E.unsafeSqlValue "row_number() over(order by update_time desc)"
        return
          (
            rowNum
          , tblFreeFrame E.^. TblFreeFrameId
          , tblFreeFrame E.^. TblFreeFrameName
          , tblFreeFrame E.^. TblFreeFrameValidFlag
          , tblFreeFrame E.^. TblFreeFramePublishDate
          , tblFreeFrame E.^. TblFreeFrameCreateTime
          , tblFreeFrame E.^. TblFreeFrameUpdateTime
          , tblFreeFrame E.^. TblFreeFrameVersion
          )
      baseQueryPage = do
        r <- baseQuery
        E.offset (pagePerLine * (intToInt64 pageNum - 1))
        E.limit pagePerLine
        return r
  list <- E.select baseQueryPage
  totalCnt <- E.select countQuery
  return $ Right (E.unValue $ totalCnt P.!! 0, toFreeFrameList <$> list)

getFreeFrameFromId ::
  FreeFrameId
  -> Handler (HResult FreeFrame)
getFreeFrameFromId (FreeFrameId freeFrameId) = runDB $ do
  case freeFrameId of
    (Just ffid) -> do
      frame <- get $ toTblFreeFrameKey ffid
      case frame of
        Just f -> return $ Right $ 
                    FreeFrame {
                        unFreeFrameId = ffid
                      , unFreeFrameName = tblFreeFrameName f
                      , unFreeFrameHtml = tblFreeFrameHtml f
                      , unFreeFrameCss = tblFreeFrameCss f
                      , unFreeFrameValidFlag = tblFreeFrameValidFlag f
                      , unFreeFramePublishDate = tblFreeFramePublishDate f
                      , unFreeFrameCreateTime = tblFreeFrameCreateTime f
                      , unFreeFrameUpdateTime = tblFreeFrameUpdateTime f
                      , unFreeFrameVersion = tblFreeFrameVersion f
                      }
        _ -> return $ Left ErrRecNotFound
    Nothing -> return $ Left ErrParam

updateFreeFrame ::
  FreeFrame
  -> Handler (HResult Int64)
updateFreeFrame frame = runDB $ do
  let frameKey = toTblFreeFrameKey $ unFreeFrameId frame
  now <- liftIO getTm
  curRec <- get frameKey
  res <- case curRec of
    Nothing -> return $ Left ErrRecNotFound
    Just record -> do
      let pver = unFreeFrameVersion frame
          dver = tblFreeFrameVersion record
      case checkVersion dver pver of
        False -> return $ Left ErrRecVersion
        _ -> do
          update frameKey [
              TblFreeFrameName =. unFreeFrameName frame
            , TblFreeFrameHtml =. unFreeFrameHtml frame
            , TblFreeFrameCss =. unFreeFrameCss frame
            , TblFreeFrameUpdateTime =. now
            , TblFreeFrameVersion +=. 1
            ]
          return $ Right (unFreeFrameId frame)
  return res

registerFreeFrame ::
  FreeFrame
  -> Handler (HResult Int64)
registerFreeFrame frame = runDB $ do
  now <- liftIO getTm
  TblFreeFrameKey (SqlBackendKey freeFrameId) <- insert $
    TblFreeFrame {
      tblFreeFrameName = unFreeFrameName frame
      , tblFreeFrameHtml = unFreeFrameHtml frame
      , tblFreeFrameCss = unFreeFrameCss frame
      , tblFreeFrameValidFlag = unFreeFrameValidFlag frame
      , tblFreeFramePublishDate = unFreeFramePublishDate frame
      , tblFreeFrameCreateTime = now
      , tblFreeFrameUpdateTime = now
      , tblFreeFrameVersion = 1
      }
  return (Right freeFrameId)

updateFreeFrameValidFlag ::
  Key TblFreeFrame
  -> RecVersion
  -> Handler (HResult Int64)
updateFreeFrameValidFlag frameKey recver = runDB $ do
  r <- get frameKey
  res <- case r of
    Nothing -> return $ Left ErrRecNotFound
    Just record -> do
      let pver = unRecVersion recver
          dver = tblFreeFrameVersion record
          freeFrameId = unSqlBackendKey (unTblFreeFrameKey frameKey)
      case checkVersion dver pver of
        False -> return $ Left ErrRecVersion
        True -> do
          now <- liftIO getTm
          updateWhere [
            TblFreeFrameValidFlag ==. True
            ] [
              TblFreeFrameValidFlag =. False
            , TblFreeFrameUpdateTime =. now
            , TblFreeFrameVersion +=. 1
            ]
          update frameKey $ [
              TblFreeFrameValidFlag =. True
            , TblFreeFrameUpdateTime =. now
            , TblFreeFrameVersion +=. 1
            ]
          return $ Right freeFrameId
  return res

deleteFreeFrame ::
  Key TblFreeFrame
  -> RecVersion
  -> Handler (HResult Int64)
deleteFreeFrame frameKey recver =  runDB $ do
  curRec <- get frameKey
  res <- case curRec of
    Nothing -> return $ Left ErrRecNotFound
    Just frame -> do
      let pver = unRecVersion recver
          dver = tblFreeFrameVersion frame
          frameId = unSqlBackendKey (unTblFreeFrameKey frameKey)
      case checkVersion dver pver of
        False -> return $ Left ErrRecVersion
        True -> case tblFreeFrameValidFlag frame of
                  True -> return $ Left ErrNotDelete
                  False -> delete frameKey >>= \_ -> return $ Right frameId
  return res

readFreeFrame ::
  Handler (HResult FreeFrame)
readFreeFrame = runDB $ do
  f <- selectFirst [ TblFreeFrameValidFlag ==. True ] []
  case f of
    Just frame -> return $ Right (toFreeFrame frame)
    _ -> return $ Left ErrRecNotFound
