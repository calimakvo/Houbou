{-# LANGUAGE OverloadedStrings #-}

module UrlParam.UserId
  (
    UserId(..)
  , convertUserId
  , encodeUserIdParam
  , decodeUserIdParam
  ) where

import Data.Int
import Data.Text
import Data.Aeson
import Data.ByteString.Internal
import Data.ByteString.Lazy (toStrict)
import Yesod.Core.Dispatch (PathPiece(..))

data UserId = UserId { _unUserId :: Maybe Int64 } deriving(Eq, Show, Read)

instance FromJSON UserId where
  parseJSON = withObject "UserId" $ \val -> UserId <$> val .: "_unUserId"

instance ToJSON UserId where
  toJSON (UserId uid) = object [ "type" .= ("UserId" :: Text), "_unUserId" .= uid ]

instance PathPiece UserId where
  toPathPiece userId = maybe "1" (pack . show) (_unUserId userId)
  fromPathPiece userId = case reads $ unpack userId of
    (uid, ""):_
      | uid > 0 -> Just $ UserId { _unUserId = Just uid }
      | otherwise -> Nothing
    _ -> Nothing

convertUserId ::
  UserId
  -> Int64
convertUserId userId = maybe 1 id (_unUserId userId)

encodeUserIdParam ::
  UserId
  -> Text
encodeUserIdParam = pack . unpackChars . toStrict . encode

decodeUserIdParam ::
  Text
  -> Maybe UserId
decodeUserIdParam = decodeStrict . packChars . unpack
