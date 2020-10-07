{-# LANGUAGE OverloadedStrings #-}

module UrlParam.Page
  (
    Page(..)
  , convertPage
  , encodePageParam
  , decodePageParam
  ) where

import Data.Text
import Data.Aeson
import Data.ByteString.Internal
import Data.ByteString.Lazy (toStrict)
import Yesod.Core.Dispatch (PathPiece(..))

data Page = Page { _unPage :: Maybe Int } deriving(Eq, Show, Read)

instance FromJSON Page where
  parseJSON = withObject "Page" $ \val -> Page <$> val .: "_unPage"

instance ToJSON Page where
  toJSON (Page page) = object [ "type" .= ("Page" :: Text), "_unPage" .= page ]

instance PathPiece Page where
  toPathPiece page = maybe "1" (pack . show) (_unPage page)
  fromPathPiece page = case reads $ unpack page of
    (p, ""):_
      | p > 1 -> Just $ Page { _unPage = Just p }
      | otherwise -> Just $ Page { _unPage = Just 1 }
    _ -> Just $ Page { _unPage  = Just 1 }

convertPage ::
  Page
  -> Int
convertPage pageInfo = maybe 1 id (_unPage pageInfo)

encodePageParam ::
  Page
  -> Text
encodePageParam = pack . unpackChars . toStrict . encode

decodePageParam ::
  Text
  -> Maybe Page
decodePageParam = decodeStrict . packChars . unpack
