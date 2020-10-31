{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Service.Preview (
    createPrevPost
  , createPrevFree
  ) where

import Import
import DataTypes.HoubouType
import Service.Common

createPrevPost :: Post -> Handler Post
createPrevPost defPost = do
  res <- liftIO $ convertMarkdownContents (unPostContent defPost) (unPostInputType defPost)
  case res of
    Just html -> return $
      defPost {
          unPostContent = html
        , unPostHtml = Just html
        }
    Nothing -> return $
      defPost { unPostHtml = Just $ unPostContent defPost }

createPrevFree :: Free -> Handler Free
createPrevFree defFree = do
  res <- liftIO $ convertMarkdownContents (unFreeContent defFree) (unFreeInputType defFree)
  case res of
    Just html -> return $
      defFree {
          unFreeContent = html
        , unFreeHtml = Just html
        }
    Nothing -> return $
      defFree { unFreeHtml = Just $ unFreeContent defFree }
