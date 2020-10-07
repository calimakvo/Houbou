{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.UserForm (
    userForm
  , UserForm(..)
  ) where

import Import
import DataTypes.HoubouType
import Forms.FormValid
import Forms.CommonForm
import Service.Common

data UserForm = UserForm {
    unUserFormId :: Int64
  , unUserFormEmail :: Text
  , unUserFormUsername :: Text
  , unUserFormProfile :: Text
  , unUserFormUserPermId :: Int64
  , unUserFormPasswd :: Maybe Text
  , unUserFormPasswdConf :: Maybe Text
  , unUserFormVersion :: Int
} deriving(Eq, Show)

userForm ::
  (Maybe User)
  -> Html
  -> MForm Handler (FormResult UserForm, Widget)
userForm user extra = do
  master <- getYesod
  xs <- liftHandler $ getTblMstUserPerm
  let emailLen = appUserAccEmailMaxLength $ appSettings master
      passwdMinLen = appUserAccPasswdMinLength $ appSettings master
      passwdMaxLen = appUserAccPasswdMaxLength $ appSettings master
      usernameLen = appUserAccUserNameMaxLength $ appSettings master
      profileLen = appUserAccUserProfMaxLength $ appSettings master
      userId = fromMaybe 0 (unUserId <$> user)
      version = fromMaybe 0 (unUserVersion <$> user)
      perms = userPermTouple xs
  (userIdRes, userIdView) <- mreq hiddenId64Field userIdFieldSet (Just userId)
  (emailRes, emailView) <- mreq (userEmailField emailLen) emailFieldSet (unUserEmail <$> user)
  (usernameRes, usernameView) <- mreq (usernameField usernameLen) usernameFieldSet (unUserUsername <$> user)
  (profileRes, profileView) <- mreq (profileField profileLen) profileFieldSet (unUserProfile <$> user)
  (permRes, permView) <- mreq (selectFieldList perms) userPermSelectFieldSet (unUserPermId <$> user)
  (pRes, passwdView) <- mopt (userPasswdField passwdMinLen passwdMaxLen) passwdFieldSet Nothing
  (pcRes, passwdConfView) <- mopt (userVerifyPasswdField pRes) passwdConfFieldSet Nothing
  (verRes, verView) <- mreq hiddenIdField versionFieldSet (Just version)
  let formParam = UserForm
                    <$> userIdRes
                    <*> emailRes
                    <*> usernameRes
                    <*> profileRes
                    <*> permRes
                    <*> pRes
                    <*> pcRes
                    <*> verRes
      widget = $(whamletFile "templates/user_form.hamlet")
  return (formParam, widget)
