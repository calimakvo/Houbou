{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.UserList where

import Import
import DataTypes.HoubouType
import UrlParam.Page
import UrlParam.UserId
import Libs.Common
import Libs.CommonWidget
import Service.Common
import Service.User
import Forms.ListSelectForm

getUserListR ::
  Page
  -> Handler Html
getUserListR page = do
  (usrKey, _) <- requireAuthPair
  master <- getYesod
  msg <- getMessages
  token <- getRequest >>= createCsrfTokenTag
  chk <- eitherToBool =<< checkAdminUserPerm usrKey
  case chk of
    False -> genError "管理権限がありません"
    True -> do
      let defLine = defSelectorValue master
      pagePerLine <- textToInt <$> (fromMaybe defLine) <$> (lookupSession userparline)
      let pageNum = fromMaybe 1 (_unPage page)
          pageInfo = PageInfo pageNum pagePerLine
      (userPerLineWidget, _) <- generateFormPost $ listSelectForm (Just pagePerLine)
      (totalCnt, users) <- eitherToTouple =<< getUserList pageInfo
      defaultLayout $ do
        let pager = paginator pageNum totalCnt pagePerLine
            ownId = fromTblUserKey usrKey
        setTitle "ユーザー"
        $(widgetFile "userlist")

postUserListR ::
  Page
  -> Handler Html
postUserListR _ = do
  ((res, _), _) <- runFormPost $ listSelectForm Nothing
  case res of
    FormSuccess (ListSelectForm pagePerLine) -> do
      setSession userparline $ pack(show(pagePerLine))
    _ -> return ()
  redirect $ UserListR (Page Nothing)
