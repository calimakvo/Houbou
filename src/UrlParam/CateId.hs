{-# LANGUAGE OverloadedStrings #-}

module UrlParam.CateId
  (
    CateId(..)
  , convertCateId
  , encodeCateIdParam
  , decodeCateIdParam
  ) where

import Data.Int
import Data.Text
import Data.Aeson
import Data.ByteString.Internal
import Data.ByteString.Lazy (toStrict)
import Yesod.Core.Dispatch (PathPiece(..))

data CateId = CateId { _unUrlCateId :: Maybe Int64 } deriving(Eq, Show, Read)

instance FromJSON CateId where
  parseJSON = withObject "CateId" $ \val -> CateId <$> val .: "_unUrlCateId"

instance ToJSON CateId where
  toJSON (CateId fid) = object [
      "type" .= ("CateId" :: Text)
    , "_unUrlCateId" .= fid
    ]

instance PathPiece CateId where
  toPathPiece frameId = maybe "0" (pack . show) (_unUrlCateId frameId)
  fromPathPiece frameId = case reads $ unpack frameId of
    (fid, ""):_
      | fid >= 0 -> Just $ CateId { _unUrlCateId = Just fid }
      | otherwise -> Nothing
    _ -> Nothing

convertCateId ::
  CateId
  -> Int64
convertCateId frameId = maybe 0 id (_unUrlCateId frameId)

encodeCateIdParam ::
  CateId
  -> Text
encodeCateIdParam = pack . unpackChars . toStrict . encode

decodeCateIdParam ::
  Text
  -> Maybe CateId
decodeCateIdParam = decodeStrict . packChars . unpack
