{-# LANGUAGE OverloadedStrings #-}

module UrlParam.YPath
  (
    YPath(..)
  , convertYPath
  , encodeYPathParam
  , decodeYPathParam
  ) where

import Data.Text
import Data.Aeson
import Data.ByteString.Internal
import Data.ByteString.Lazy (toStrict)
import Yesod.Core.Dispatch (PathPiece(..))

data YPath = YPath { _unUrlYPath :: Maybe Text } deriving(Eq, Show, Read)

instance FromJSON YPath where
  parseJSON = withObject "YPath" $ \val -> YPath <$> val .: "_unUrlYPath"

instance ToJSON YPath where
  toJSON (YPath path) = object [
      "type" .= ("YPath" :: Text)
    , "_unUrlYPath" .= path
    ]

instance PathPiece YPath where
  toPathPiece path = maybe empty id (_unUrlYPath path)
  fromPathPiece path = case path of
    "" -> Nothing
    _ -> Just $ YPath { _unUrlYPath = Just path }

convertYPath ::
  YPath
  -> Text
convertYPath path = maybe empty id (_unUrlYPath path)

encodeYPathParam ::
  YPath
  -> Text
encodeYPathParam = pack . unpackChars . toStrict . encode

decodeYPathParam ::
  Text
  -> Maybe YPath
decodeYPathParam = decodeStrict . packChars . unpack
