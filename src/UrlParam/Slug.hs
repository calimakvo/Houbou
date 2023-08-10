{-# LANGUAGE OverloadedStrings #-}

module UrlParam.Slug
  (
    Slug(..)
  , convertSlug
  , encodeSlugParam
  , decodeSlugParam
  ) where

import Data.Text
import Data.Aeson
import Data.ByteString.Internal (unpackChars, packChars)
import Data.ByteString.Lazy (toStrict)
import Yesod.Core.Dispatch (PathPiece(..))

data Slug = Slug { _unUrlSlug :: Maybe Text } deriving(Eq, Show, Read)

instance FromJSON Slug where
  parseJSON = withObject "Slug" $ \val -> Slug <$> val .: "_unUrlSlug"

instance ToJSON Slug where
  toJSON (Slug slug) = object [
      "type" .= ("Slug" :: Text)
    , "_unUrlSlug" .= slug
    ]

instance PathPiece Slug where
  toPathPiece slug = maybe empty id (_unUrlSlug slug)
  fromPathPiece slug = case slug of
    "" -> Nothing
    _ -> Just $ Slug { _unUrlSlug = Just slug }

convertSlug ::
  Slug
  -> Text
convertSlug slug = maybe empty id (_unUrlSlug slug)

encodeSlugParam ::
  Slug
  -> Text
encodeSlugParam = pack . unpackChars . toStrict . encode

decodeSlugParam ::
  Text
  -> Maybe Slug
decodeSlugParam = decodeStrict . packChars . unpack
