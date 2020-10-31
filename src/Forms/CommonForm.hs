{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Forms.CommonForm (
    titleFieldSet
  , bodyFieldSet
  , slugPostFieldSet
  , slugFreeFieldSet
  , tagFieldSet
  , postIdFieldSet
  , frameIdFieldSet
  , frameNameFieldSet
  , frameHtmlFieldSet
  , frameCssFieldSet
  , freeFrameHtmlFieldSet
  , freeFrameCssFieldSet
  , freeIdFieldSet
  , freeTitleFieldSet
  , freeContFieldSet
  , freeCssFieldSet
  , versionFieldSet
  , inputTypeRadioFieldSet
  , freeFrameIdFieldSet
  , blogNameFieldSet
  , blogUrlFieldSet
  , blogMdaUrlFieldSet
  , blogMdaDirFieldSet
  , blogPostNumSelectFieldSet
  , uploadSizeFieldSet
  , lineSelectFieldSet
  , emailFieldSet
  , passwdFieldSet
  , passwdConfFieldSet
  , usernameFieldSet
  , profileFieldSet
  , lineSelectTouple
  , getInputTypeTouple
  , getStatusSelectTouple
  , postNumSelectTouple
  , userPermTouple
  , userIdFieldSet
  , userPermSelectFieldSet
  , mediaIdFieldSet
  , mediaTitleFieldSet
  , blogAdsFieldSet
  , statusTypeRadioFieldSet
  , descriptionFieldSet
  , keywordsFieldSet
  , robotsFieldSet
  , prevBodyFieldSet
  , prevTitleFieldSet
  , prevCssFieldSet
  , prevInputTypeFieldSet
  , prevTypeFieldSet
  , initFormUrlpath
  , formResultToId
  ) where

import Import
import DataTypes.HoubouType
import qualified Data.List as L
import qualified Data.Text as T

titleFieldSet :: FieldSettings master
titleFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "postTitleInputId"
  , fsName = Just "post_title"
  , fsAttrs = [
        ("class", "form-control")
      , ("placeholder", "ブログタイトル")
    ]
  }

bodyFieldSet :: FieldSettings master
bodyFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "postTextInputId"
  , fsName = Just "post_text"
  , fsAttrs = [
        ("style", "resize: vertical;")
      , ("class", "form-control")
      , ("placeholder", "ブログ本文")
      , ("rows", "15")
    ]
  }

slugPostFieldSet :: FieldSettings master
slugPostFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "slugPostInputId"
  , fsName = Just "post_slug"
  , fsAttrs = [
        ("class", "form-control")
      , ("placeholder", "スラッグ")
    ]
  }

slugFreeFieldSet :: FieldSettings master
slugFreeFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "slugFreeInputId"
  , fsName = Just "free_slug"
  , fsAttrs = [
        ("class", "form-control")
      , ("placeholder", "スラッグ")
    ]
  }

tagFieldSet :: FieldSettings master
tagFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "tagsInputId"
  , fsName = Just "tags"
  , fsAttrs = [
        ("class", "form-control")
      , ("placeholder", "ブログタグ、カンマ区切りで入力")
    ]
  }

postIdFieldSet :: FieldSettings master
postIdFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "postId"
  , fsName = Just "post_id"
  , fsAttrs = []
 }

frameIdFieldSet :: FieldSettings master
frameIdFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "frameId"
  , fsName = Just "frame_id"
  , fsAttrs = []
 }

frameNameFieldSet :: FieldSettings master
frameNameFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "frameNameId"
  , fsName = Just "frame_name"
  , fsAttrs = [
        ("class", "form-control")
      , ("placeholder", "フレーム名")
    ]
  }

frameHtmlFieldSet :: FieldSettings master
frameHtmlFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "frameHtmlId"
  , fsName = Just "frame_html"
  , fsAttrs = [
        ("style", "resize: vertical;")
      , ("class", "form-control")
      , ("placeholder", "フレームHTML")
      , ("rows", "15")
    ]
 }

frameCssFieldSet :: FieldSettings master
frameCssFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "frameCssId"
  , fsName = Just "frame_css"
  , fsAttrs = [
        ("style", "resize: vertical;")
      , ("class", "form-control")
      , ("placeholder", "フレームCSS")
      , ("rows", "15")
    ]
 }

freeFrameHtmlFieldSet :: FieldSettings master
freeFrameHtmlFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "freeFrameHtmlId"
  , fsName = Just "free_frame_html"
  , fsAttrs = [
        ("style", "resize: vertical;")
      , ("class", "form-control")
      , ("placeholder", "フレームHTML")
      , ("rows", "15")
    ]
 }

freeFrameCssFieldSet :: FieldSettings master
freeFrameCssFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "freeFrameCssId"
  , fsName = Just "free_frame_css"
  , fsAttrs = [
        ("style", "resize: vertical;")
      , ("class", "form-control")
      , ("placeholder", "フレームCSS")
      , ("rows", "15")
    ]
 }

versionFieldSet :: FieldSettings master
versionFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "versionId"
  , fsName = Just "version"
  , fsAttrs = []
 }

freeIdFieldSet :: FieldSettings master
freeIdFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "freeId"
  , fsName = Just "free_id"
  , fsAttrs = []
 }

freeTitleFieldSet :: FieldSettings master
freeTitleFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "freeTitleInputId"
  , fsName = Just "free_title"
  , fsAttrs = [
        ("class", "form-control")
      , ("placeholder", "フリーページタイトル")
    ]
  }

freeContFieldSet :: FieldSettings master
freeContFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "freeContentInputId"
  , fsName = Just "free_content"
  , fsAttrs = [
        ("style", "resize: vertical;")
      , ("class", "form-control")
      , ("placeholder", "フリーページコンテンツ")
      , ("rows", "15")
    ]
 }

freeCssFieldSet :: FieldSettings master
freeCssFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "freeCssInputId"
  , fsName = Just "freepage_css"
  , fsAttrs = [
        ("style", "resize: vertical;")
      , ("class", "form-control")
      , ("placeholder", "フリーページCSS")
      , ("rows", "15")
    ]
 }

inputTypeRadioFieldSet :: FieldSettings master
inputTypeRadioFieldSet = FieldSettings
    {
      fsLabel = ""
    , fsTooltip = Nothing
    , fsId = Just "inputTypeId"
    , fsName = Just "input_type"
    , fsAttrs =
      [
          ("class", "")
      ]
    }

statusTypeRadioFieldSet :: FieldSettings master
statusTypeRadioFieldSet = FieldSettings
    {
      fsLabel = ""
    , fsTooltip = Nothing
    , fsId = Just "statusTypeId"
    , fsName = Just "status_type"
    , fsAttrs =
      [
          ("class", "")
      ]
    }

freeFrameIdFieldSet :: FieldSettings master
freeFrameIdFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "freeFrameId"
  , fsName = Just "freeframe_id"
  , fsAttrs = []
 }

blogNameFieldSet :: FieldSettings master
blogNameFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "blogNameId"
  , fsName = Just "blog_name"
  , fsAttrs =
    [
      ("class", "form-control")
      ]
 }

blogAdsFieldSet :: FieldSettings master
blogAdsFieldSet =  FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "adsTxtId"
  , fsName = Just "ads_txt"
  , fsAttrs =
    [
      ("class", "form-control")
    ]
 }

blogUrlFieldSet :: FieldSettings master
blogUrlFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "blogUrlId"
  , fsName = Just "blog_url"
  , fsAttrs =
    [
      ("class", "form-control")
    , ("type", "url")
    ]
 }

blogMdaUrlFieldSet :: FieldSettings master
blogMdaUrlFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "blogImgUrlId"
  , fsName = Just "blog_img_url"
  , fsAttrs =
    [
      ("class", "form-control")
    , ("type", "url")
    ]
 }

blogMdaDirFieldSet :: FieldSettings master
blogMdaDirFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "blogImgDirId"
  , fsName = Just "blog_img_dir"
  , fsAttrs =
    [
      ("class", "form-control")
    ]
 }

uploadSizeFieldSet :: FieldSettings master
uploadSizeFieldSet  = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "uploadSizeId"
  , fsName = Just "upload_size"
  , fsAttrs =
    [
      ("class", "form-control")
    ]
  }

blogPostNumSelectFieldSet :: FieldSettings master
blogPostNumSelectFieldSet  = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "postNumId"
  , fsName = Just "post_num"
  , fsAttrs =
    [
      ("class", "form-control")
    ]
  }

lineSelectFieldSet :: FieldSettings master
lineSelectFieldSet  = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "pageperline"
  , fsName = Just "pageperline"
  , fsAttrs = [
      ("class", "form-control")
      ]
  }

emailFieldSet :: FieldSettings master
emailFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "userEmailId"
  , fsName = Just "user_email"
  , fsAttrs = [
      ("class", "form-control")
      ]
  }

passwdFieldSet :: FieldSettings master
passwdFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "userPasswdId"
  , fsName = Just "user_passwd"
  , fsAttrs = [
      ("class", "form-control")
      , ("autocomplete","new-password")
      ]
  }

passwdConfFieldSet :: FieldSettings master
passwdConfFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "userPasswdConfId"
  , fsName = Just "user_passwd_conf"
  , fsAttrs = [
      ("class", "form-control")
      ]
  }

usernameFieldSet :: FieldSettings master
usernameFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "userNameId"
  , fsName = Just "user_name"
  , fsAttrs = [
      ("class", "form-control")
      ]
  }

profileFieldSet :: FieldSettings master
profileFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "userProfileId"
  , fsName = Just "user_profile"
  , fsAttrs = [
      ("class", "form-control")
      ]
  }

userIdFieldSet ::  FieldSettings master
userIdFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "userId"
  , fsName = Just "user_id"
  , fsAttrs = [
      ("class", "form-control")
      ]
  }

userPermSelectFieldSet :: FieldSettings master
userPermSelectFieldSet  = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "userPermSelectId"
  , fsName = Just "user_perm_id"
  , fsAttrs = [
      ("class", "form-control")
      ]
  }

mediaIdFieldSet :: FieldSettings master
mediaIdFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "mediaId"
  , fsName = Just "media_id"
  , fsAttrs = []
 }

mediaTitleFieldSet :: FieldSettings master
mediaTitleFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "mediaTitleInputId"
  , fsName = Just "media_title"
  , fsAttrs = [
        ("class", "form-control")
      , ("placeholder", "メディアタイトル")
    ]
  }

descriptionFieldSet :: FieldSettings master
descriptionFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "descriptionInputId"
  , fsName = Just "meta_description"
  , fsAttrs = [
        ("class", "form-control")
      , ("placeholder", "meta description")
      , ("rows", "10")
    ]
  }

keywordsFieldSet :: FieldSettings master
keywordsFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "keywordsInputId"
  , fsName = Just "meta_keywords"
  , fsAttrs = [
        ("class", "form-control")
      , ("placeholder", "meta keywords")
    ]
  }

robotsFieldSet :: FieldSettings master
robotsFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "robotsInputId"
  , fsName = Just "meta_robots"
  , fsAttrs = [
        ("class", "form-control")
      , ("placeholder", "meta index,follow")
    ]
  }

prevBodyFieldSet :: FieldSettings master
prevBodyFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "prevInputBodyId"
  , fsName = Just "preview_body"
  , fsAttrs = []
  }

prevTitleFieldSet :: FieldSettings master
prevTitleFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "prevInputTitleId"
  , fsName = Just "preview_title"
  , fsAttrs = []
  }

prevCssFieldSet :: FieldSettings master
prevCssFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "prevInputCssId"
  , fsName = Just "preview_css"
  , fsAttrs = []
  }

prevInputTypeFieldSet :: FieldSettings master
prevInputTypeFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "prevInputTypeId"
  , fsName = Just "input_type"
  , fsAttrs = []
  }

prevTypeFieldSet :: FieldSettings master
prevTypeFieldSet = FieldSettings {
    fsLabel = ""
  , fsTooltip = Nothing
  , fsId = Just "prevPreviewTypeId"
  , fsName = Just "preview_type"
  , fsAttrs = []
  }

getInputTypeTouple ::
  Handler [(T.Text, Int)]
getInputTypeTouple = return $ [("markdown", 1), ("html", 2)]

getStatusSelectTouple ::
  Handler [(T.Text, Int)]
getStatusSelectTouple = return $ [
    ("非公開", fromEnum UnPublished)
  , ("公開", fromEnum Published)
  ]

postNumSelectTouple ::
  Int
  -> Handler [(T.Text, Int)]
postNumSelectTouple maxNum = return $
  map (first $ T.pack . show) (L.zip [1..maxNum] [1..maxNum])

lineSelectTouple ::
  [Int]
  -> Handler [(T.Text, Int)]
lineSelectTouple xs = return $ L.zip (map (T.pack . show) xs) xs

userPermTouple ::
  [MstUserPerm]
  -> [(T.Text, Int64)]
userPermTouple = map (\x -> (unMstUserName x, unMstUserPermId x))

initFormUrlpath ::
  FormResult (Maybe Text)
  -> (Maybe Text)
  -> FormResult (Maybe Text)
initFormUrlpath slugRes urlpath =
  case slugRes of
    FormSuccess Nothing -> FormSuccess Nothing
    FormSuccess (Just _) -> FormSuccess urlpath
    _ -> FormSuccess Nothing

formResultToId :: FormResult Int64 -> Int64
formResultToId formId =
  case formId of
    FormSuccess i -> i
    _ -> 0
