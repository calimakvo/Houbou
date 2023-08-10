{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Service.Media (
    getMediaList
  , registMedia
  , deleteMedia
  , getMediaFromId
  , updateMedia
  ) where

import Import
import System.Directory
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Digest.Pure.MD5 as M
import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.ByteString as B
import Database.Persist.Sql (BackendKey(..))
import qualified Database.Esqueleto.Internal.Internal as IE
import qualified Database.Esqueleto.Experimental as E
import DataTypes.HoubouType
import Libs.Common
import Service.Common
import UrlParam.MediaId

type TFilePath = Text
type TFileName = Text
type TTitleName = Text

getMediaList ::
  PageInfo ->
  Handler (HResult (Int, [MediaList]))
getMediaList pageInfo = runDB $ do
  let pageNum = unPageNum pageInfo
      pagePerLine = fromIntegral $ unPagePerLine pageInfo
      countQuery = do
        tblMedia <- E.from $ E.table @TblMedia
        return $ E.count(tblMedia E.^. TblMediaId)
      baseQuery = do
        let rowNum = IE.unsafeSqlValue "row_number() over(order by tbl_media.create_time desc)"
        (tblMedia E.:& tblMstMimeType) <-
          E.from $ E.table @TblMedia
          `E.LeftOuterJoin` E.table @TblMstMimeType
          `E.on` (\(tblMedia E.:& tblMstMimeType) ->
                    tblMedia E.^. TblMediaMimeTypeId E.==. tblMstMimeType E.?. TblMstMimeTypeId)
        E.orderBy [E.desc (tblMedia E.^. TblMediaCreateTime)]
        return
          (
            rowNum
          , tblMedia E.^. TblMediaId
          , tblMedia E.^. TblMediaTitle
          , tblMedia E.^. TblMediaDir
          , tblMedia E.^. TblMediaFileName
          , tblMedia E.^. TblMediaSize
          , tblMedia E.^. TblMediaThumbDispFlag
          , tblMedia E.^. TblMediaCreateTime
          , tblMedia E.^. TblMediaUpdateTime
          , tblMstMimeType E.?. TblMstMimeTypeExtension
          , tblMstMimeType E.?. TblMstMimeTypeMimeType
          , tblMedia E.^. TblMediaVersion
          )
      baseQueryPage = do
        r <- baseQuery
        E.offset (pagePerLine * (intToInt64 pageNum - 1))
        E.limit pagePerLine
        return r
  list <- E.select baseQueryPage
  totalCnt <- E.select countQuery
  return $ Right (E.unValue $ totalCnt P.!! 0, toMediaList <$> list)

registMedia ::
  Key TblUser
  -> BlogSetting
  -> [TTitleName]
  -> [FileInfo]
  -> Handler (HResult [Int64])
registMedia usrKey setting titles files = do
  (dateDir, fnames) <- registerUploadFile setting files
  let upfs = zip3 titles files fnames
  mediaIds <- runDB $ do
    mapM (\(t, f, fn) -> do
      bs <- fileSourceByteString f
      size <- (pure bs) >>= sizeCalc
      contentType <- liftHandler $ eitherToMaybe =<< getContentTypeOne (fileContentType f)
      now <- liftIO getTm
      TblMediaKey (SqlBackendKey mediaId) <- insert $
        TblMedia {
            tblMediaTitle = toJustTitle t
          , tblMediaDir = rmSlash(dateDir <> "/") -- 20200902/
          , tblMediaFileName = fn                 -- filename.ext
          , tblMediaHash = Just $ toText (M.md5 (LC.fromStrict bs))
          , tblMediaSize = size
          , tblMediaThumbDispFlag = thumbDispFlagFromMime contentType
          , tblMediaMimeTypeId = (toTblMstMimeTypeKey . unMstMimeTypeId) <$> contentType
          , tblMediaCreateTime = now
          , tblMediaUpdateTime = now
          , tblMediaVersion = 1
          , tblMediaAuthorId = usrKey
          }
      return mediaId) upfs
  return $ Right mediaIds

getMediaFromId ::
  MediaId
  -> Handler (HResult Media)
getMediaFromId (MediaId mediaId) = runDB $ do
  res <- case mediaId of
    (Just mid) -> do
      media <- get $ toTblMediaKey mid
      case media of
        Just m -> return $ Right $
          Media {
              unMediaId = mid
            , unMediaTitle = tblMediaTitle m
            , unMediaDir = tblMediaDir m
            , unMediaFileName = tblMediaFileName m
            , unMediaHash = tblMediaHash m
            , unMediaSize = tblMediaSize m
            , unMediaThumbDispFlag = tblMediaThumbDispFlag m
            , unMediaMimeTypeId = fromTblMstMimeTypeKey <$> (tblMediaMimeTypeId m)
            , unMediaCreateTime = tblMediaCreateTime m
            , unMediaUpdateTime = tblMediaUpdateTime m
            , unMediaVersion = tblMediaVersion m
            , unMediaAuthorId = fromTblUserKey $ tblMediaAuthorId m
            }
        _ -> return $ Left ErrRecNotFound
    Nothing -> return $ Left ErrParam
  return res

updateMedia ::
  Media
  -> Handler (HResult Int64)
updateMedia media = runDB $ do
  let mediaKey = toTblMediaKey $ unMediaId media
  curRec <- get mediaKey
  res <- case curRec of
    Nothing -> return $ Left ErrRecNotFound
    Just record -> do
      let pver = unMediaVersion media
          dver = tblMediaVersion record
      case checkVersion dver pver of
        False -> return $ Left ErrRecVersion
        True -> do
          now <- liftIO getTm
          update mediaKey [
              TblMediaTitle =. unMediaTitle media
            , TblMediaUpdateTime =. now
            , TblMediaVersion +=. 1
            ]
          return $ Right (unMediaId media)
  return res

deleteMedia ::
  Key TblMedia
  -> BlogSetting
  -> RecVersion
  -> Handler (HResult Int64)
deleteMedia mediaKey setting recver = runDB $ do
  curRec <- get mediaKey
  res <- case curRec of
    Nothing -> return $ Left ErrRecNotFound
    Just media -> do
      let pver = unRecVersion recver
          dver = tblMediaVersion media
          mediaId = unSqlBackendKey (unTblMediaKey mediaKey)
      case checkVersion dver pver of
        False -> return $ Left ErrRecVersion
        True -> do
          rmres <- liftHandler $ removeMedia (toMedia (Entity mediaKey media)) setting
          case rmres of
              Right _ -> do
                delete mediaKey >>= \_ -> return $ Right mediaId
              Left err -> return $ Left err
  return res

removeMedia ::
  Media
  -> BlogSetting
  -> Handler (HResult TFilePath)
removeMedia media setting = do
  let mdaDir = unBlogSettingMediaDir setting
      dir  = unMediaDir media
      name = unMediaFileName media
      rmFile = rmSlash(mdaDir <> "/" <> dir <> "/" <> name)
      rmFile' = T.unpack rmFile
      imgParh = T.unpack $ rmSlash(mdaDir <> "/" <> dir)
  exist <- liftIO $ doesFileExist rmFile'
  case exist of
    False -> return $ Left ErrFileNotFound
    True -> do
      liftIO $ removeFile rmFile'
      res <- liftIO $ isDirectoryEmpty imgParh
      case res of
        True -> do
          liftIO $ removeDirectory imgParh
          return $ Right rmFile
        False -> return $ Right rmFile

isDirectoryEmpty ::
  FilePath
  -> IO Bool
isDirectoryEmpty imgPath =
  getDirectoryContents imgPath >>= return . (== [".", ".."])
  
registerUploadFile ::
  BlogSetting
  -> [FileInfo]
  -> Handler (TFilePath, [TFileName]) 
registerUploadFile setting files = do
  dateDir <- liftIO $ dateFormatNonSlash <$> getTm
  let mdaTop = unBlogSettingMediaDir setting
      mdaDirTop = rmSlash(mdaTop <> dateDir <> "/") -- /pub/images/20200902/
      dir = T.unpack mdaDirTop
  exist <- liftIO $ doesDirectoryExist dir
  case exist of
    False -> liftIO $ createDirectory dir
    True -> return ()
  fnames <- writeFiles mdaDirTop files
  return (dateDir, fnames)

writeFiles :: MonadResource m =>
  TFilePath
  -> [FileInfo]
  -> m [TTitleName]
writeFiles dir files = do
  fnames <- mapM (\f -> (fileSourceByteString f) >>= wtiteUploadFile dir (fileName f)) files
  return fnames

wtiteUploadFile :: MonadResource m =>
  TFilePath
  -> TFilePath
  -> ByteString
  -> m TTitleName
wtiteUploadFile d f bs = do
  let filename = T.unpack f
      dir = T.unpack d
      xs = T.unpack <$> T.splitOn (T.pack ".") f
      fn = xs P.!! 0
      ext = xs P.!! 1
  curDir <- liftIO $ getCurrentDirectory
  liftIO $ setCurrentDirectory dir
  exist <- liftIO $ doesFileExist filename
  case exist of
    False -> do
      liftIO $ B.writeFile (dir ++ filename) bs
      liftIO $ setCurrentDirectory curDir
      return $ pack filename
    True -> do
      timeStr <- T.unpack <$> (liftIO $ dateFormatNonSlashTime <$> getTm)
      let fname = fn ++ "_" ++ timeStr ++ "." ++ ext
      liftIO $ B.writeFile (dir ++ fname) bs
      liftIO $ setCurrentDirectory curDir
      return $ pack fname

getContentTypeOne ::
  Text
  -> Handler (Either ErrHoubou MstMimeType)
getContentTypeOne contentType = runDB $ do
  result <- selectFirst [ TblMstMimeTypeMimeType ==. contentType,
                          TblMstMimeTypeDeleteFlag ==. False ] []
  case result of
    Just record -> return $ Right (toMstMimeType record)
    _ -> return $ Left ErrRecNotFound

sizeCalc :: MonadResource m =>
  ByteString
  -> m Int
sizeCalc = return . length

thumbDispFlagFromMime ::
  Maybe MstMimeType
  -> Bool
thumbDispFlagFromMime contentType =
  case contentType of
    Nothing -> False
    Just m -> elem (unMstMimeTypeExtension m) ["png", "jpg", "jpeg", "jpe", "gif", "svg"]

toJustTitle ::
  TTitleName
  -> Maybe TTitleName
toJustTitle title
  | null title == True = Nothing
  | otherwise = Just title
