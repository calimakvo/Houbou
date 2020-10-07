{-# LANGUAGE OverloadedStrings #-}

module UrlParam.MediaId
  (
    MediaId(..)
  , convertMediaId
  , encodeMediaIdParam
  , decodeMediaIdParam
  ) where

import Data.Int
import Data.Text
import Data.Aeson
import Data.ByteString.Internal
import Data.ByteString.Lazy (toStrict)
import Yesod.Core.Dispatch (PathPiece(..))

data MediaId = MediaId { _unUrlMediaId :: Maybe Int64 } deriving(Eq, Show, Read)

instance FromJSON MediaId where
  parseJSON = withObject "MediaId" $ \val -> MediaId <$> val .: "_unUrlMediaId"

instance ToJSON MediaId where
  toJSON (MediaId mid) = object [
      "type" .= ("MediaId" :: Text)
    , "_unUrlMediaId" .= mid
    ]

instance PathPiece MediaId where
  toPathPiece mediaId = maybe "1" (pack . show) (_unUrlMediaId mediaId)
  fromPathPiece mediaId = case reads $ unpack mediaId of
    (mid, ""):_
      | mid > 0 -> Just $ MediaId { _unUrlMediaId = Just mid }
      | otherwise -> Nothing
    _ -> Nothing

convertMediaId ::
  MediaId
  -> Int64
convertMediaId mediaId = maybe 1 id (_unUrlMediaId mediaId)

encodeMediaIdParam ::
  MediaId
  -> Text
encodeMediaIdParam = pack . unpackChars . toStrict . encode

decodeMediaIdParam ::
  Text
  -> Maybe MediaId
decodeMediaIdParam = decodeStrict . packChars . unpack
