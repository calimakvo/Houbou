{-# LANGUAGE OverloadedStrings #-}

module UrlParam.FreeFrameId
  (
    FreeFrameId(..)
  , convertFreeFrameId
  , encodeFreeFrameIdParam
  , decodeFreeFrameIdParam
  ) where

import Data.Int
import Data.Text
import Data.Aeson
import Data.ByteString.Internal
import Data.ByteString.Lazy (toStrict)
import Yesod.Core.Dispatch (PathPiece(..))

data FreeFrameId = FreeFrameId { _unUrlFreeFrameId :: Maybe Int64 } deriving(Eq, Show, Read)

instance FromJSON FreeFrameId where
  parseJSON = withObject "FreeFrameId" $ \val -> FreeFrameId <$> val .: "_unUrlFreeFrameId"

instance ToJSON FreeFrameId where
  toJSON (FreeFrameId ffid) = object [ "type" .= ("FreeFrameId" :: Text), "_unUrlFreeFrameId" .= ffid ]

instance PathPiece FreeFrameId where
  toPathPiece freeFrameId = maybe "1" (pack . show) (_unUrlFreeFrameId freeFrameId)
  fromPathPiece freeFrameId = case reads $ unpack freeFrameId of
    (ffid, ""):_
      | ffid > 0 -> Just $ FreeFrameId { _unUrlFreeFrameId = Just ffid }
      | otherwise -> Nothing
    _ -> Nothing

convertFreeFrameId :: FreeFrameId -> Int64
convertFreeFrameId freeFrameId = maybe 1 id (_unUrlFreeFrameId freeFrameId)

encodeFreeFrameIdParam :: FreeFrameId -> Text
encodeFreeFrameIdParam = pack . unpackChars . toStrict . encode

decodeFreeFrameIdParam :: Text -> Maybe FreeFrameId
decodeFreeFrameIdParam = decodeStrict . packChars . unpack
