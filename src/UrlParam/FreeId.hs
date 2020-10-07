{-# LANGUAGE OverloadedStrings #-}

module UrlParam.FreeId
  (
    FreeId(..)
  , convertFreeId
  , encodeFreeIdParm
  , decodeFreeIdParm
  ) where

import Data.Int
import Data.Text
import Data.Aeson
import Data.ByteString.Internal
import Data.ByteString.Lazy (toStrict)
import Yesod.Core.Dispatch (PathPiece(..))

data FreeId = FreeId { _unUrlFreeId :: Maybe Int64 } deriving(Eq, Show, Read)

instance FromJSON FreeId where
  parseJSON = withObject "FreeId" $ \val -> FreeId <$> val .: "_unUrlFreeId"

instance ToJSON FreeId where
  toJSON (FreeId fpId) = object [ "type" .= ("FreeId" :: Text), "_unUrlFreeId" .= fpId ]

instance PathPiece FreeId where
  toPathPiece fpId = maybe "1" (pack . show) (_unUrlFreeId fpId)
  fromPathPiece fpId = case reads $ unpack fpId of
    (fid, ""):_
      | fid > 0 -> Just $ FreeId { _unUrlFreeId = Just fid }
      | otherwise -> Nothing
    _ -> Nothing

convertFreeId ::
  FreeId
  -> Int64
convertFreeId fpId = maybe 1 id (_unUrlFreeId fpId)

encodeFreeIdParm ::
  FreeId
  -> Text
encodeFreeIdParm = pack . unpackChars . toStrict . encode

decodeFreeIdParm ::
  Text
  -> Maybe FreeId
decodeFreeIdParm = decodeStrict . packChars . unpack
