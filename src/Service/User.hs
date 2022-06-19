{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Service.User (
    getUserList
  , checkAdminUserPerm
  , getUser
  , updateUser
  , registerUser
  , deleteUser
  ) where

import Import
import Data.Maybe
import qualified Prelude as P
import Database.Persist.Sql (BackendKey(..))
import qualified Database.Esqueleto.Internal.Internal as IE
import qualified Database.Esqueleto.Experimental as E
import Libs.Common
import DataTypes.HoubouType
import Service.Common

getUserList ::
  PageInfo ->
  Handler (HResult (Int, [UserList]))
getUserList pageInfo = runDB $ do
  let pageNum = unPageNum pageInfo
      pagePerLine = fromIntegral $ unPagePerLine pageInfo
      countQuery = do
        tblUser <- E.from $ E.table @TblUser
        return $ E.count(tblUser E.^. TblUserId)
      baseQuery = do
        let rowNum = IE.unsafeSqlValue "row_number() over(order by tbl_user.create_time asc)"
        (tblUser E.:& tblMstUserPerm) <-
          E.from $ E.table @TblUser
          `E.InnerJoin` E.table @TblMstUserPerm
          `E.on` (\(tblUser E.:& tblMstUserPerm) ->
                    tblUser E.^. TblUserUserPermId E.==. tblMstUserPerm E.^. TblMstUserPermId)
        E.orderBy [E.asc (tblUser E.^. TblUserCreateTime)]
        return
          (
            rowNum
          , tblUser E.^. TblUserId
          , tblUser E.^. TblUserUsername
          , tblUser E.^. TblUserEmail
          , tblUser E.^. TblUserCreateTime
          , tblUser E.^. TblUserUpdateTime
          , tblUser E.^. TblUserVersion
          , tblUser E.^. TblUserDeleteFlag
          , tblMstUserPerm E.^. TblMstUserPermLevel
          , tblMstUserPerm E.^. TblMstUserPermName
          )
      baseQueryPage = do
        r <- baseQuery
        E.offset (pagePerLine * (intToInt64 pageNum - 1))
        E.limit pagePerLine
        return r
  list <- E.select baseQueryPage
  totalCnt <- E.select countQuery
  return $ Right (E.unValue $ totalCnt P.!! 0, toUserList <$> list)

checkAdminUserPerm ::
  Key TblUser
  -> Handler (HResult Bool)
checkAdminUserPerm loginUserKey = runDB $ do
  let
    baseQuery = do
      (tblUser E.:& tblMstUserPerm) <-
        E.from $ E.table @TblUser
        `E.InnerJoin` E.table @TblMstUserPerm
        `E.on` (\(tblUser E.:& tblMstUserPerm) ->
                  tblUser E.^. TblUserUserPermId E.==. tblMstUserPerm E.^. TblMstUserPermId)
      E.where_ $ do
        tblUser E.^. TblUserId E.==. E.val loginUserKey
          E.&&. tblMstUserPerm E.^. TblMstUserPermLevel E.==. E.val 0
      return (tblUser E.^. TblUserId)
  list <- E.select baseQuery
  if length list == 1 then return (Right True) else return (Right False)

getUser ::
  Key TblUser
  -> Handler (HResult User)
getUser userKey = runDB $ do
  result <- get userKey
  case result of
    Nothing -> return $ Left ErrRecNotFound
    Just user -> return $ Right (toUser (Entity userKey user))

updateUser ::
  User
  -> Handler (HResult Int64)
updateUser user = runDB $ do
  let userKey = toTblUserKey $ unUserId user
      rawPass = unUserPasswd user
  curRec <- get userKey
  res <- case curRec of
    Nothing -> return $ Left ErrRecNotFound
    Just record -> do
      let pver = unUserVersion user
          dver = tblUserVersion record
      case checkVersion dver pver of
        False -> return $ Left ErrRecVersion
        True -> do
          upPass <- case rawPass of
                      Just rp -> do
                        encPass <- liftIO $ encryptPasswd defaultStrength rp
                        return [ TblUserPassword =. Just encPass ]
                      Nothing -> return []
          now <- liftIO getTm
          update userKey ([
              TblUserUsername =. unUserUsername user
            , TblUserEmail =. unUserEmail user
            , TblUserProfile =. unUserProfile user
            , TblUserUserPermId =. toTblMstUserPermKey (unUserPermId user)
            , TblUserUpdateTime =. now
            , TblUserVersion +=. 1
            ] ++ upPass)
          return $ Right (fromTblUserKey userKey)
  return res

registerUser :: User -> Handler (HResult Int64)
registerUser user = runDB $ do
  let rawPass = fromJust $ unUserPasswd user
  now <- liftIO getTm
  encPass <- liftIO $ encryptPasswd defaultStrength rawPass
  TblUserKey (SqlBackendKey userId) <- insert $
    TblUser{
        tblUserUsername = unUserUsername user
      , tblUserPassword = Just encPass
      , tblUserEmail = unUserEmail user
      , tblUserProfile = unUserProfile user
      , tblUserCreateTime = now
      , tblUserUpdateTime = now
      , tblUserVersion = 1
      , tblUserUserPermId = toTblMstUserPermKey (unUserPermId user)
      , tblUserDeleteFlag = False
      }
  return $ Right userId

deleteUser ::
     Bool
  -> Key TblUser
  -> RecVersion
  -> Handler (HResult Int64)
deleteUser flag userKey recver = runDB $ do
  curRec <- get userKey
  res <- case curRec of
    Nothing -> return $ Left ErrRecNotFound
    Just user -> do
      let pver = unRecVersion recver
          dver = tblUserVersion user
          userId = fromTblUserKey userKey
      case checkVersion dver pver of
        False -> return $ Left ErrRecVersion
        True -> do
          now <- liftIO getTm
          update userKey [
              TblUserUpdateTime =. now
            , TblUserVersion +=. 1
            , TblUserDeleteFlag =. flag
            ]
          return $ Right userId
  return res
