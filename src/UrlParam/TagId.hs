{-# LANGUAGE OverloadedStrings #-}

module UrlParam.TagId
  (
    TagId(..)
  , convertTagId
  , encodeTagIdParam
  , decodeTagIdParam
  ) where

import Data.Int
import Data.Text
import Data.Aeson
import Data.ByteString.Internal
import Data.ByteString.Lazy (toStrict)
import Yesod.Core.Dispatch (PathPiece(..))

data TagId = TagId { _unUrlTagId :: Maybe Int64 } deriving(Eq, Show, Read)

instance FromJSON TagId where
  parseJSON = withObject "TagId" $ \val -> TagId <$> val .: "_unUrlTagId"

instance ToJSON TagId where
  toJSON (TagId tid) = object [
      "type" .= ("TagId" :: Text)
    , "_unUrlTagId" .= tid
    ]

instance PathPiece TagId where
  toPathPiece tagId = maybe "1" (pack . show) (_unUrlTagId tagId)
  fromPathPiece tagId = case reads $ unpack tagId of
    (tid, ""):_
      | tid > 0 -> Just $ TagId { _unUrlTagId = Just tid }
      | otherwise -> Nothing
    _ -> Nothing

convertTagId ::
  TagId
  -> Int64
convertTagId tagId = maybe 1 id (_unUrlTagId tagId)

encodeTagIdParam ::
  TagId
  -> Text
encodeTagIdParam = pack . unpackChars . toStrict . encode

decodeTagIdParam ::
  Text
  -> Maybe TagId
decodeTagIdParam = decodeStrict . packChars . unpack
