{-# LANGUAGE OverloadedStrings #-}

module UrlParam.PostId
  (
    PostId(..)
  , convertPostId
  , encodePostIdParam
  , decodePostIdParam
  ) where

import Data.Int
import Data.Text
import Data.Aeson
import Data.ByteString.Internal
import Data.ByteString.Lazy (toStrict)
import Yesod.Core.Dispatch (PathPiece(..))

data PostId = PostId { _unPostId :: Maybe Int64 } deriving(Eq, Show, Read)

instance FromJSON PostId where
  parseJSON = withObject "PostId" $ \val -> PostId <$> val .: "_unPostId"

instance ToJSON PostId where
  toJSON (PostId bid) = object [ "type" .= ("PostId" :: Text), "_unPostId" .= bid ]

instance PathPiece PostId where
  toPathPiece postId = maybe "1" (pack . show) (_unPostId postId)
  fromPathPiece postId = case reads $ unpack postId of
    (bid, ""):_
      | bid > 0 -> Just $ PostId { _unPostId = Just bid }
      | otherwise -> Nothing
    _ -> Nothing

convertPostId ::
  PostId
  -> Int64
convertPostId postId = maybe 1 id (_unPostId postId)

encodePostIdParam ::
  PostId
  -> Text
encodePostIdParam = pack . unpackChars . toStrict . encode

decodePostIdParam ::
  Text
  -> Maybe PostId
decodePostIdParam = decodeStrict . packChars . unpack
