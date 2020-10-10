{-# LANGUAGE OverloadedStrings #-}

module UrlParam.MPath
  (
    MPath(..)
  , convertMPath
  , encodeMPathParam
  , decodeMPathParam
  ) where

import Data.Text
import Data.Aeson
import Data.ByteString.Internal
import Data.ByteString.Lazy (toStrict)
import Yesod.Core.Dispatch (PathPiece(..))

data MPath = MPath { _unUrlMPath :: Maybe Text } deriving(Eq, Show, Read)

instance FromJSON MPath where
  parseJSON = withObject "MPath" $ \val -> MPath <$> val .: "_unUrlMPath"

instance ToJSON MPath where
  toJSON (MPath path) = object [
      "type" .= ("MPath" :: Text)
    , "_unUrlMPath" .= path
    ]

instance PathPiece MPath where
  toPathPiece path = maybe empty id (_unUrlMPath path)
  fromPathPiece path = case path of
    "" -> Nothing
    _ -> Just $ MPath { _unUrlMPath = Just path }

convertMPath ::
  MPath
  -> Text
convertMPath path = maybe empty id (_unUrlMPath path)

encodeMPathParam ::
  MPath
  -> Text
encodeMPathParam = pack . unpackChars . toStrict . encode

decodeMPathParam ::
  Text
  -> Maybe MPath
decodeMPathParam = decodeStrict . packChars . unpack
