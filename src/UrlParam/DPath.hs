{-# LANGUAGE OverloadedStrings #-}

module UrlParam.DPath
  (
    DPath(..)
  , convertDPath
  , encodeDPathParam
  , decodeDPathParam
  ) where

import Data.Text
import Data.Aeson
import Data.ByteString.Internal
import Data.ByteString.Lazy (toStrict)
import Yesod.Core.Dispatch (PathPiece(..))

data DPath = DPath { _unUrlDPath :: Maybe Text } deriving(Eq, Show, Read)

instance FromJSON DPath where
  parseJSON = withObject "DPath" $ \val -> DPath <$> val .: "_unUrlDPath"

instance ToJSON DPath where
  toJSON (DPath path) = object [
      "type" .= ("DPath" :: Text)
    , "_unUrlDPath" .= path
    ]

instance PathPiece DPath where
  toPathPiece path = maybe empty id (_unUrlDPath path)
  fromPathPiece path = case path of
    "" -> Nothing
    _ -> Just $ DPath { _unUrlDPath = Just path }

convertDPath ::
  DPath
  -> Text
convertDPath path = maybe empty id (_unUrlDPath path)

encodeDPathParam ::
  DPath
  -> Text
encodeDPathParam = pack . unpackChars . toStrict . encode

decodeDPathParam ::
  Text
  -> Maybe DPath
decodeDPathParam = decodeStrict . packChars . unpack
