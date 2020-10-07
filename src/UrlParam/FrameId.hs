{-# LANGUAGE OverloadedStrings #-}

module UrlParam.FrameId
  (
    FrameId(..)
  , convertFrameId
  , encodeFrameIdParam
  , decodeFrameIdParam
  ) where

import Data.Int
import Data.Text
import Data.Aeson
import Data.ByteString.Internal
import Data.ByteString.Lazy (toStrict)
import Yesod.Core.Dispatch (PathPiece(..))

data FrameId = FrameId { _unUrlFrameId :: Maybe Int64 } deriving(Eq, Show, Read)

instance FromJSON FrameId where
  parseJSON = withObject "FrameId" $ \val -> FrameId <$> val .: "_unUrlFrameId"

instance ToJSON FrameId where
  toJSON (FrameId fid) = object [
      "type" .= ("FrameId" :: Text)
    , "_unUrlFrameId" .= fid
    ]

instance PathPiece FrameId where
  toPathPiece frameId = maybe "1" (pack . show) (_unUrlFrameId frameId)
  fromPathPiece frameId = case reads $ unpack frameId of
    (fid, ""):_
      | fid > 0 -> Just $ FrameId { _unUrlFrameId = Just fid }
      | otherwise -> Nothing
    _ -> Nothing

convertFrameId ::
  FrameId
  -> Int64
convertFrameId frameId = maybe 1 id (_unUrlFrameId frameId)

encodeFrameIdParam ::
  FrameId
  -> Text
encodeFrameIdParam = pack . unpackChars . toStrict . encode

decodeFrameIdParam ::
  Text
  -> Maybe FrameId
decodeFrameIdParam = decodeStrict . packChars . unpack
