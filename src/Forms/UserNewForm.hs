{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.UserNewForm (
    userNewForm
  , UserNewForm(..)
  ) where

import Import
import DataTypes.HoubouType
import Forms.FormValid
import Forms.CommonForm
import Service.Common

data UserNewForm = UserNewForm {
    unUserNewFormId :: Int64
  , unUserNewFormEmail :: Text
  , unUserNewFormUsername :: Text
  , unUserNewFormProfile :: Text
  , unUserNewFormUserPermId :: Int64
  , unUserNewFormPasswd :: Text
  , unUserNewFormPasswdConf :: Text
  , unUserNewFormVersion :: Int
} deriving(Eq, Show)

userNewForm ::
  (Maybe User)
  -> Html
  -> MForm Handler (FormResult UserNewForm, Widget)
userNewForm _ extra = do
  master <- getYesod
  xs <- liftHandler $ getTblMstUserPerm
  let emailLen = appUserAccEmailMaxLength $ appSettings master
      passwdMinLen = appUserAccPasswdMinLength $ appSettings master
      passwdMaxLen = appUserAccPasswdMaxLength $ appSettings master
      usernameLen = appUserAccUserNameMaxLength $ appSettings master
      profileLen = appUserAccUserProfMaxLength $ appSettings master
      perms = userPermTouple xs
  (userIdRes, userIdView) <- mreq hiddenId64Field userIdFieldSet (Just 0)
  (emailRes, emailView) <- mreq (userEmailField emailLen) emailFieldSet Nothing
  (usernameRes, usernameView) <- mreq (usernameField usernameLen) usernameFieldSet Nothing
  (profileRes, profileView) <- mreq (profileField profileLen) profileFieldSet Nothing
  (permRes, permView) <- mreq (selectFieldList perms) userPermSelectFieldSet Nothing
  (pRes, passwdView) <- mreq (userPasswdField passwdMinLen passwdMaxLen) passwdFieldSet Nothing
  (pcRes, passwdConfView) <- mreq (userVerifyNewPasswdField pRes) passwdConfFieldSet Nothing
  (verRes, verView) <- mreq hiddenIdField versionFieldSet (Just 0)
  let formParam = UserNewForm
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
