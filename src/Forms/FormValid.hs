{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Forms.FormValid (
    titleField
  , bodyField
  , slugPostField
  , slugFreeField
  , tagField
  , hiddenIdField
  , hiddenId64Field
  , frameNameField
  , frameHtmlField
  , frameCssField
  , freeTitleField
  , freeContField
  , freeCssField
  , blogNameField
  , blogUrlField
  , blogMediaUrlField
  , blogMediaDirField
  , uploadSizeField
  , userEmailField
  , userPasswdField
  , usernameField
  , profileField
  , userVerifyPasswdField
  , userVerifyNewPasswdField
  , mediaTitleField
  , blogAdsField
  ) where

import Import
import Text.Zenhan
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.Email.Validate
import DataTypes.HoubouType
import Libs.Common
import Libs.Template
import Forms.CommonForm
import qualified Text.Ginger as G
import qualified Database.Esqueleto as E
import Service.Common

data SlugChkType =
    SlugRegUp
  | SlugRegNew
  | SlugRegNone deriving(Show, Eq)

titleField :: Int -> Field Handler Text
titleField len = check (titleValid len) textField

titleValid :: Int -> Text -> Either Text Text
titleValid len s
  | length(s) > len
    = Left $ ("ブログタイトルは" <> pack(show(len)) <> "文字まで入力可能です")
  | otherwise = Right s

bodyField :: Int -> Field Handler Textarea
bodyField len = checkM (bodyValid len) textareaField

bodyValid :: Int -> Textarea -> Handler (Either Text Textarea)
bodyValid len s =
  case length(unTextarea s) > len of
    True -> return $ Left $ ("記事本文は" <> pack(show(len)) <> "文字まで入力可能です")
    False -> do
      let inpTemp = unpack (unTextarea s)
      result <- liftIO $ checkTemplate inpTemp
      case result of
        Nothing -> return $ Right s
        (Just (pe, srcpos)) ->
          return $ Left $ ("記事が不正です、err=" <> pe <> " エラー内容:　" <> srcpos)

slugPostField ::
  Int
  -> FormResult Int64
  -> Maybe Text
  -> Field Handler Text
slugPostField len pid urlpath = checkM (slugPostValid len pid urlpath) textField

slugPostValid ::
  Int
  -> FormResult Int64
  -> Maybe Text
  -> Text
  -> Handler (Either Text Text)
slugPostValid len pid urlpath slug = do
  case checkLengthSlug len slug of
    False -> return $ Left ("スラッグは" <> pack(show(len)) <> "文字まで入力可能です")
    True -> case null slug of
      True -> return $ Right slug
      False -> runDB $ do
        let postId = formResultToId pid
            key = TblPostKey (E.SqlBackendKey postId)
        p <- if postId > 0 then do
               res <- get key
               case res of
                 Just p' -> return $ Just $ toPost (Entity key p')
                 Nothing -> return Nothing
             else
               return Nothing
        case checkUpdateSlugType p slug of
          SlugRegNew -> do -- 新規登録
            r <- getBy $ UniTblPostSlugUrlpath urlpath (Just slug)
            case r of
              Just _ -> return $ Left ("パーマリンクが重複しています")
              Nothing -> return $ Right slug
          SlugRegUp -> do
            r <- getBy $ UniTblPostSlugUrlpath urlpath (Just slug)
            case r of
              Just (Entity k' _) ->
                if (unPostId <$> p) == (Just $ fromTblPostKey k') then
                  return $ Right slug
                else
                  return $ Left ("パーマリンクが重複しています")
              Nothing -> return $ Right slug
          SlugRegNone -> return $ Right slug

checkUpdateSlugType :: Maybe a -> Text -> SlugChkType
checkUpdateSlugType p s =
  if not (null s) && isNothing p then
    SlugRegNew
  else if not (null s) && isJust p then
    SlugRegUp
  else
    SlugRegNone

slugFreeField ::
  Int
  -> FormResult Int64
  -> Maybe Text
  -> Field Handler Text
slugFreeField len fid urlpath = checkM (slugFreeValid len fid urlpath) textField

slugFreeValid ::
  Int
  -> FormResult Int64
  -> Maybe Text
  -> Text
  -> Handler (Either Text Text)
slugFreeValid len fid urlpath slug = do
  case checkLengthSlug len slug of
    False -> return $ Left ("スラッグは" <> pack(show(len)) <> "文字まで入力可能です")
    True -> case null slug of
      True -> return $ Right slug
      False -> runDB $ do
        let freeId = formResultToId fid
            key = TblFreeKey (E.SqlBackendKey freeId)
        f <- if freeId > 0 then do
               res <- get key
               case res of
                 Just f' -> return $ Just $ toFree (Entity key f')
                 Nothing -> return Nothing
             else
               return Nothing
        case checkUpdateSlugType f slug of
          SlugRegNew -> do
            r <- getBy $ UniTblFreeSlugUrlpath urlpath (Just slug)
            case r of
              Just _ -> return $ Left ("パーマリンクが重複しています")
              Nothing -> return $ Right slug
          SlugRegUp -> do
            r <- getBy $ UniTblFreeSlugUrlpath urlpath (Just slug)
            case r of
              Just (Entity k' _) ->
                if (unFreeId <$> f) == (Just $ fromTblFreeKey k') then
                  return $ Right slug
                else
                  return $ Left ("パーマリンクが重複しています")
              Nothing -> return $ Right slug
          SlugRegNone -> return $ Right slug

tagField :: Int -> Int -> Field Handler Text
tagField len oneLen = check (tagValid len oneLen) textField

tagValid :: Int -> Int -> Text -> Either Text Text
tagValid len oneLen s =
  case length(s) > len of
    True -> Left $ ("タグは" <> pack(show(len)) <> "文字まで入力可能です")
    False -> case res of
      True -> Right $ h2z [Kana] T.empty (z2h [Digit, Ascii] T.empty (T.intercalate (T.pack ",") tagList))
      False -> Left $ "1つのタグの長さは" <> toText(oneLen) <> "までです (" <> failTag <> ")"
      where tagList = filter (/= T.empty) $ T.splitOn (T.pack ",") s
            res = all (\s' -> T.length(s') <= oneLen) tagList
            failTag = T.intercalate (T.pack ",") $ filter (\s' -> T.length(s') > oneLen) tagList
    
hiddenIdField :: Field Handler Int
hiddenIdField = check hiddenIdValid hiddenField

hiddenIdValid :: Int -> Either Text Int
hiddenIdValid serial
  | serial >= 0 = Right serial
  | otherwise = Left "パラメータが不正です"

hiddenId64Field :: Field Handler Int64
hiddenId64Field = check hiddenId64Valid hiddenField

hiddenId64Valid :: Int64 -> Either Text Int64
hiddenId64Valid serial
  | serial >= 0 = Right serial
  | otherwise = Left "パラメータが不正です"

uploadSizeField :: Int -> Field Handler Int
uploadSizeField upsize = check (uploadSizeValid upsize) intField

uploadSizeValid :: Int -> Int -> Either Text Int
uploadSizeValid upsize s = if s >= uploadSizeMin && s < upsize then Right s else sizeerr
  where sizeerr = Left $ ("アップロードサイズは" <> pack(show(upsize)) <> "MBまで入力可能です")

frameNameField :: Int -> Field Handler Text
frameNameField len = check (frameNameValid len) textField

frameNameValid :: Int -> Text -> Either Text Text
frameNameValid len s
  | length(s) > len
    = Left $ ("フレーム名は" <> pack(show(len)) <> "文字まで入力可能です")
  | otherwise = Right s

frameHtmlField :: Int -> Field Handler Textarea
frameHtmlField len = checkM (frameHtmlValid len) textareaField
  
frameHtmlValid :: Int -> Textarea -> Handler (Either Text Textarea)
frameHtmlValid len s = do
  case length(unTextarea s) > len of
    True -> return $ Left $ ("フレームのHTMLは" <> pack(show(len)) <> "文字まで入力可能です")
    False -> do
      let inpTemp = unpack (unTextarea s)
      result <- liftIO $ checkTemplate inpTemp
      case result of
        Nothing -> return $ Right s
        (Just (pe, srcpos)) ->
          return $ Left $ ("フレームのテンプレートが不正です、err=" <> pe <> " エラー内容:　" <> srcpos)

frameCssField :: Int -> Field Handler Textarea
frameCssField len = check (frameCssValid len) textareaField

frameCssValid :: Int -> Textarea -> Either Text Textarea
frameCssValid len s
  | length(unTextarea s) > len
    = Left $ ("フレームのCSSは" <> pack(show(len)) <> "文字まで入力可能です")
  | otherwise = Right s

freeTitleField :: Int -> Field Handler Text
freeTitleField len = check (freeTitleValid len) textField

freeTitleValid :: Int -> Text -> Either Text Text
freeTitleValid len s
  | length(s) > len
    = Left $ ("フリーページ名は" <> pack(show(len)) <> "文字まで入力可能です")
  | otherwise = Right s

freeContField :: Int -> Field Handler Textarea
freeContField len = checkM (freeContValid len) textareaField

freeContValid :: Int -> Textarea -> Handler (Either Text Textarea)
freeContValid len s =
  case length(unTextarea s) > len of
    True -> return $ Left $ ("フリーページのHTMLは" <> pack(show(len)) <> "文字まで入力可能です")
    False -> do
      let inpTemp = unpack (unTextarea s)
      result <- liftIO $ checkTemplate inpTemp
      case result of
        Nothing -> return $ Right s
        (Just (pe, srcpos)) ->
          return $ Left $ ("フリーページが不正です、err=" <> pe <> " エラー内容:　" <> srcpos)

freeCssField :: Int -> Field Handler Textarea
freeCssField len = check (freeCssValid len) textareaField

freeCssValid :: Int -> Textarea -> Either Text Textarea
freeCssValid len s
  | length(unTextarea s) > len
    = Left $ ("フリーページのCSSは" <> pack(show(len)) <> "文字まで入力可能です")
  | otherwise = Right s

blogNameField :: Int -> Field Handler Text
blogNameField len = check (blogNameValid len) textField

blogAdsField :: Int -> Field Handler Text
blogAdsField len = check (blogAdsValid len) textField

blogAdsValid :: Int -> Text -> Either Text Text
blogAdsValid len s
  | length(s) > len
    = Left $ ("adstxtは" <> pack(show(len)) <> "文字まで入力可能です")
  | otherwise = Right s

blogNameValid :: Int -> Text -> Either Text Text
blogNameValid len s
  | length(s) > len
    = Left $ ("サイト名は" <> pack(show(len)) <> "文字まで入力可能です")
  | otherwise = Right s

blogUrlField :: Int -> Field Handler Text
blogUrlField len = check (blogUrlValid len) textField

blogUrlValid :: Int -> Text -> Either Text Text
blogUrlValid len s
  | length(s) > len
    = Left $ ("サイトURLは" <> pack(show(len)) <> "文字まで入力可能です")
  | otherwise = Right s

blogMediaUrlField :: Int -> Field Handler Text
blogMediaUrlField len = check (blogMediaUrlValid len) textField

blogMediaUrlValid :: Int -> Text -> Either Text Text
blogMediaUrlValid len s
  | length(s) > len
    = Left $ ("画像URLは" <> pack(show(len)) <> "文字まで入力可能です")
  | otherwise = Right s

blogMediaDirField :: Int -> Field Handler Text
blogMediaDirField len = check (blogMediaDirValid len) textField

blogMediaDirValid :: Int -> Text -> Either Text Text
blogMediaDirValid len s
  | length(s) > len
    = Left $ ("画像ディレクトリは" <> pack(show(len)) <> "文字まで入力可能です")
  | otherwise = Right s

userEmailField :: Int -> Field Handler Text
userEmailField len = check (userEmailValid len) textField

userEmailValid :: Int -> Text -> Either Text Text
userEmailValid len s
  | length(s) > len
    = Left $ ("メールアドレスは" <> pack(show(len)) <> "文字まで入力可能です")
  | (isValid $ TE.encodeUtf8(s)) == False
    = Left $ "メールアドレスの書式を確認してください"
  | otherwise = Right s

userPasswdField :: Int -> Int -> Field Handler Text
userPasswdField pmin pmax = check(userPasswdValid pmin pmax) passwordField

userPasswdValid :: Int -> Int -> Text -> Either Text Text
userPasswdValid pmin pmax s
  | (length(s) >= pmin && length(s) <= pmax) = Right s
  | otherwise = Left $ ("パスワードは" <> pack(show(pmin)) <> "から" <>  pack(show(pmax)) <> "文字まで入力可能です")

userVerifyPasswdField :: FormResult (Maybe Text) -> Field Handler Text
userVerifyPasswdField res = check(userVerifyPasswdValid res) passwordField

userVerifyPasswdValid :: FormResult (Maybe Text) -> Text -> Either Text Text
userVerifyPasswdValid res s =
    case res of
      FormSuccess Nothing -> if null s
                             then Right s
                             else Left "パスワードが合致しません"
      FormSuccess (Just pass) -> if s == pass
                                 then Right s
                                 else Left "パスワードが合致しません"
      _ -> Right s

userVerifyNewPasswdField :: FormResult Text -> Field Handler Text
userVerifyNewPasswdField res = check(userVerifyNewPasswdValid res) passwordField

userVerifyNewPasswdValid :: FormResult Text -> Text -> Either Text Text
userVerifyNewPasswdValid res s =
  case res of
    FormSuccess pass -> if s == pass
                        then Right s
                        else Left "パスワードが合致しません"
    _ -> Right s

usernameField :: Int -> Field Handler Text
usernameField len = check(usernameValid len) textField

usernameValid :: Int -> Text -> Either Text Text
usernameValid len s
  | length(s) > len
    = Left $ ("ユーザー名は" <> pack(show(len)) <> "文字まで入力可能です")
  | otherwise = Right s

profileField ::  Int -> Field Handler Text
profileField len = check(profileValid len) textField

profileValid :: Int -> Text -> Either Text Text
profileValid len s
  | length(s) > len
    = Left $ ("ユーザープロファイルは" <> pack(show(len)) <> "文字まで入力可能です")
  | otherwise = Right s

mediaTitleField :: Int -> Field Handler Text
mediaTitleField len = check (mediaTitleValid len) textField

mediaTitleValid :: Int -> Text -> Either Text Text
mediaTitleValid len s
  | length(s) > len
    = Left $ ("メディアタイトルは" <> pack(show(len)) <> "文字まで入力可能です")
  | otherwise = Right s

checkTemplate :: String -> IO (Maybe (Text, Text))
checkTemplate s = do
  template <- createTemplate Nothing s
  case template of
    Right _ -> return Nothing
    Left err -> do
      let pe = pack (G.peErrorMessage err)
          srcpos = pack $ fromMaybe "なし" (show <$> G.peSourcePosition err)
      return $ Just (pe, srcpos)

checkLengthSlug :: Int -> Text -> Bool
checkLengthSlug len' s = if length s > len' then False else True
