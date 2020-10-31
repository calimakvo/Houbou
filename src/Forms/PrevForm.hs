{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Forms.PrevForm (
    prevForm
  , PrevForm(..)
  , encodePrevFormParm
  , decodePrevFormParm
  , toPrevForm
  ) where

import Import
import Data.Aeson
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy as D
import Libs.Common
import Forms.CommonForm

data PrevForm = PrevForm {
    unPrevFormPreviewTiele :: Maybe Text
  , unPrevFormPreviewContent :: Maybe Text
  , unPrevFormPreviewCss :: Maybe Text
  , unPrevFormPreviewInputType :: Int
  , unPrevFormPreviewType :: Text
  , unPrevFormCreateTime :: UTCTime
  , unPrevFormUpdateTime :: UTCTime
} deriving(Eq, Show, Read)

prevForm :: Html -> MForm Handler (FormResult PrevForm, Widget)
prevForm _ = do
  now <- liftIO getTm
  (titleRes, _) <- mopt textField prevTitleFieldSet Nothing
  (contRes, _) <- mopt textField prevBodyFieldSet Nothing
  (cssRes, _) <- mopt textField prevCssFieldSet Nothing
  (inptypeRes, _) <- mreq intField prevInputTypeFieldSet Nothing
  (typeRes, _) <- mreq textField prevTypeFieldSet Nothing
  let formParam = PrevForm
                    <$> titleRes
                    <*> contRes
                    <*> cssRes
                    <*> inptypeRes
                    <*> typeRes
                    <*> FormSuccess now
                    <*> FormSuccess now
      widget = return ()
  return (formParam, widget)

instance FromJSON PrevForm where
  parseJSON = withObject "PrevForm" $ \val ->
    PrevForm <$> val .: "unPrevFormPreviewTiele"
             <*> val .: "unPrevFormPreviewContent"
             <*> val .: "unPrevFormPreviewCss"
             <*> val .: "unPrevFormPreviewInputType"
             <*> val .: "unPrevFormPreviewType"
             <*> val .: "unPrevFormCreateTime"
             <*> val .: "unPrevFormUpdateTime"

instance ToJSON PrevForm where
  toJSON (PrevForm title content css inptype prevtype ct ut) = object 
    [   "type" .= ("PrevForm" :: Text)
      , "unPrevFormPreviewTiele" .= title
      , "unPrevFormPreviewContent" .= content
      , "unPrevFormPreviewCss" .= css
      , "unPrevFormPreviewInputType" .= inptype
      , "unPrevFormPreviewType" .= prevtype
      , "unPrevFormCreateTime" .= ct
      , "unPrevFormUpdateTime" .= ut
    ]

encodePrevFormParm ::
  PrevForm
  -> Text
encodePrevFormParm = pack . unpackChars . D.toStrict . encode

decodePrevFormParm ::
  Text
  -> Maybe PrevForm
decodePrevFormParm = decodeStrict . packChars . unpack

toPrevForm :: Text -> PrevForm
toPrevForm f =
  case decodePrevFormParm f of
    Just form -> form
    Nothing -> error "Preview: form param error"
