{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Html (
      createBlogContents
    , createBlogFreeContents
    , createTagListContents
    , createPrevBlogContents
    , createPrevBlogFreeContents
  ) where

import Import
import qualified Prelude as P
import Data.Maybe
import qualified Data.Text as T
import Text.Regex.PCRE
import Text.Blaze.Html
import qualified Data.HashMap.Strict as H
import DataTypes.HoubouType
import UrlParam.PostId
import UrlParam.FreeId
import UrlParam.TagId
import Libs.Common
import Libs.CommonWidget
import Libs.Template
import Service.Common
import Service.BlogSetting
import Service.Frame
import Service.FreeFrame
import Service.Post
import Service.Free
import Service.Tags
import qualified GHC.Exts as GE
import qualified Text.Ginger as G

createBlogContents ::
  Text
  -> PostId
  -> Handler (HResult Html)
createBlogContents setId postId = do
  setting <- getBlogSetting setId
  frame <- eitherToMaybe =<< readPostFrame
  posts <- eitherToList =<< (getPublishPostFromId setting postId)
  posted <- eitherToList =<< getPublishPostedList
  tags <- eitherToList =<< getTagList
  case (frame /= Nothing && null posts == False) of
    False -> do
      $(logInfo) $ "createBlogContents': frame or posts undefined"
      return $ Left ErrPostUnitialize
    True -> do
      html <- renderBlogContents setting (fromJust frame) posts posted tags
      return html

renderBlogContents ::
  BlogSetting
  -> Frame
  -> [Post]
  -> [Post]
  -> [MstTag]
  -> Handler (HResult Html)
renderBlogContents setting frame posts posted tags = do
  let pageMeta = toFramePageMeta setting (posts P.!! 0) frame
      postedElms = toPostToPosted posted
      renderInfo' = initContextPost pageMeta postedElms tags
  ppost <- liftIO $ preRenderPost posts renderInfo'
  template <- liftIO $ createTemplate Nothing (unpack $ internalComp frame)
  case template of
    Left err -> do
      $(logInfo) $ "renderBlogContents': template parse error = " <> toText(err)
      return $ Left ErrTemplateUnInit
    Right template' -> do
      let renderInfo = setContextPost renderInfo' pageMeta ppost
      return $ Right $ preEscapedToHtml (render template' renderInfo)

preRenderPost ::
  [Post]
  -> HashMap G.VarName Value
  -> IO [Post]
preRenderPost posts renderInfo = do
  ps <- mapM (\p -> do
      template <- createTemplate Nothing (unpack $ getPutHtml p)
      case template of
        Left err -> do
          error $ "preRenderPost: post template parse error post_id=" ++
                    show(unPostId p) ++ " err=" ++ show(err)
        Right template' -> do
          return $ p { unPostHtml = Just (render template' renderInfo) }) posts
  return ps
                 
createBlogFreeContents ::
  Text
  -> FreeId
  -> Handler (HResult Html)
createBlogFreeContents setId freeId = do
  setting <- getBlogSetting setId
  frame <- eitherToMaybe =<< readFreeFrame
  frees <- eitherToList =<< (getPublishFreeFromId setting freeId)
  posted <- eitherToList =<< getPublishPostedList
  tags <- eitherToList =<< getTagList
  case (frame /= Nothing && null frees == False) of
    False -> return $ Left ErrFreeUnitialize
    True -> do
      html <- renderBlogFreeContents setting (fromJust frame) frees posted tags
      return html

renderBlogFreeContents ::
  BlogSetting
  -> FreeFrame
  -> [Free]
  -> [Post]
  -> [MstTag]
  -> Handler (HResult Html)
renderBlogFreeContents setting frame frees posted tags = do
  let pageMeta = toFreeFramePageMeta setting (frees P.!! 0) frame
      postedElms = toPostToPosted posted
      renderInfo' = initContextFree pageMeta postedElms tags
  pfree <- liftIO $ preRenderFree frees renderInfo'
  template <- liftIO $ createTemplate Nothing (unpack $ internalFreeComp frame)
  case template of
    Left err -> do
      $(logInfo) $ "createBlogFreeContents': template parse error = " <> toText(err)
      return $ Left ErrTemplateUnInit
    Right template' -> do
      let renderInfo = setContextFree renderInfo' pageMeta pfree
      return $ Right $ preEscapedToHtml (render template' renderInfo)

preRenderFree ::
  [Free]
  -> HashMap G.VarName Value
  -> IO [Free]
preRenderFree frees renderInfo = do
  ps <- mapM (\f -> do
      template <- createTemplate Nothing (unpack $ getPutHtmlFree f)
      case template of
        Left err -> do
          error $ "preRenderFree: free template parse error free_id="
                    ++ show(unFreeId f)
                    ++ " err=" ++ show(err)
        Right template' -> do
          return $ f { unFreeHtml = Just (render template' renderInfo) }) frees
  return ps

createTagListContents ::
  Text
  -> TagId
  -> Handler (HResult Html)
createTagListContents setId tagId = do
  setting <- getBlogSetting setId
  frame <- eitherToMaybe =<< readFreeFrame
  msttag <- eitherToMaybe =<< getMstTagFromId tagId
  tagconts <- eitherToList =<< getTagContents tagId
  posted <- eitherToList =<< getPublishPostedList
  tags <- eitherToList =<< getTagList
  case (frame /= Nothing && null tagconts == False && isJust msttag == True) of
    False -> do
      $(logInfo) $ "createTagListContents': tag data invalid."
      return $ Left ErrTagContUnitialize
    True -> do
      html <- renderTagListContents' setting (fromJust frame) tagconts
                                       posted tags (fromJust msttag)
      return html

renderTagListContents' ::
  BlogSetting
  -> FreeFrame
  -> [TagContent]
  -> [Post]
  -> [MstTag]
  -> MstTag
  -> Handler (HResult Html)
renderTagListContents' setting frame tagconts posted tags msttag = do
  template <- liftIO $ createTemplate Nothing (unpack $ internalFreeComp frame)
  case template of
    Left err -> do
      $(logInfo) $ "createTagContents': template parse error = " <> toText(err)
      return $ Left ErrTemplateUnInit
    Right template' -> do
      return $ Right $ preEscapedToHtml
        (render template' (initContextTagCont pageMeta
                           tagconts postedElms tags msttag))
  where
    pageMeta = toTagContsFramePageMeta setting frame
    postedElms = toPostToPosted posted

internalFreeComp ::
  FreeFrame
  -> Text
internalFreeComp f =
  let frm = maybeToText $ unFreeFrameHtml f
      css = maybeToText $ unFreeFrameCss f
  in T.replace "<!-- HB_CSS -->" css frm

internalComp ::
  Frame
  -> Text
internalComp f =
  let frm = maybeToText $ unFrameHtml f
      css = maybeToText $ unFrameCss f
  in T.replace "<!-- HB_CSS -->" css frm

initContextTagCont ::
  PageMeta
  -> [TagContent]
  -> [Posted]
  -> [MstTag]
  -> MstTag
  -> H.HashMap Text Value
initContextTagCont pageMeta tagcont postedElms tags msttag = H.fromList [
    ("hb_blog_name", String $ unPageMetaBlogName pageMeta)
  , ("hb_blog_title", String $ unPageMetaTitle pageMeta)
  , ("hb_search_tag", String $ unMstTagName msttag)
  , ("hb_blog_url", String $ unPageMetaBlogUrl pageMeta)
  , ("hb_blog_css", String $ unPageMetaFrameCss pageMeta)
  , ("hb_share_url", initTagListShareUrl msttag pageMeta)
  , ("hb_blog_media_url", String $ unPageMetaMediaUrl pageMeta)
  , ("hb_tagconts", Array $ fromList (initTagContList tagcont))
  , ("hb_posteds", Array $ fromList (initPostedList postedElms))
  , ("hb_tags", Array $ fromList (initMstTagList tags)) ]
  
initContextFree ::
  PageMeta
  -> [Posted]
  -> [MstTag]
  -> H.HashMap Text Value
initContextFree pageMeta postedElms tags = H.fromList [
    ("hb_blog_name", String $ unPageMetaBlogName pageMeta)
  , ("hb_blog_title", String $ unPageMetaTitle pageMeta)
  , ("hb_blog_url", String $ unPageMetaBlogUrl pageMeta)
  , ("hb_blog_css", String $ unPageMetaFrameCss pageMeta)
  , ("hb_blog_canonical_url", String $ unPageMetaCanonicalUrl pageMeta)
  , ("hb_blog_media_url", String $ unPageMetaMediaUrl pageMeta)
  , ("hb_posteds", Array $ fromList (initPostedList postedElms))
  , ("hb_meta_description", String $ unPageMetaDescription pageMeta)
  , ("hb_meta_keywords", String $ unPageMetaKeywords pageMeta)
  , ("hb_meta_robots", String $ unPageMetaRobots pageMeta)
  , ("hb_tags", Array $ fromList (initMstTagList tags))
  , ("hb_meta_og_image", String $ unPageMetaOgImg pageMeta)
  , ("hb_meta_og_title", String $ unPageMetaOgTitle pageMeta)
  , ("hb_meta_og_url", String $ unPageMetaOgUrl pageMeta)
  , ("hb_meta_og_site_name", String $ unPageMetaOgSiteName pageMeta)
  , ("hb_meta_og_desc", String $ unPageMetaOgDesc pageMeta)
  , ("hb_meta_og_page_type", String $ unPageMetaOgPageType pageMeta)
  ]

initTagContList ::
  [TagContent]
  -> [Value]
initTagContList tagconts =
  map (\tagcont ->
         case tagcont of
           TagContPost taginfo -> createObjectTagPost taginfo
           TagContFree taginfo -> createObjectTagFree taginfo
      ) tagconts
  where
    pubdate ut = toGrecoFullLocal (utcToJpTime ut)
    createObjectTagPost :: TagInfo -> Value
    createObjectTagPost tinfo =
      Object (
        GE.fromList [
                ("type", Number $ fromIntegral (fromEnum PostTag))
              , ("id", Number (fromIntegral $ unTagInfoId tinfo))
              , ("title", String $ unTagInfoTitle tinfo)
              , ("posted", zoneTimeToValue (pubdate (unTagInfoPosted tinfo)))
              ] )
    createObjectTagFree :: TagInfo -> Value
    createObjectTagFree tinfo =
      Object (
        GE.fromList [
                 ("type", Number $ fromIntegral (fromEnum FreeTag))
               , ("id", Number (fromIntegral $ unTagInfoId tinfo))
               , ("title", String $ unTagInfoTitle tinfo)
               , ("posted", zoneTimeToValue (pubdate (unTagInfoPosted tinfo)))
               ] )


initFreeList ::
  [Free]
  -> [Value]
initFreeList frees =
  map (\free ->
          let d = toGrecoFullLocal $ fromJust (utcToJpTime <$> unFreePublishDate free)
          in
            Object (
              GE.fromList [
                    ("hb_body", String $ getPutHtmlFree free)
                  , ("hb_css",  String $ (maybeToText $ unFreeCss free))
                  , ("hb_publish_date",  zoneTimeToValue d)
                  ]
              )) frees

initContextPost ::
  PageMeta
  -> [Posted]
  -> [MstTag]
  -> H.HashMap Text Value
initContextPost pageMeta postedElms tags = H.fromList [
    ("hb_blog_name", String $ unPageMetaBlogName pageMeta)
  , ("hb_blog_title", String $ unPageMetaTitle pageMeta)
  , ("hb_blog_url", String $ unPageMetaBlogUrl pageMeta)
  , ("hb_blog_css", String $ unPageMetaFrameCss pageMeta)
  , ("hb_blog_canonical_url", String $ unPageMetaCanonicalUrl pageMeta)
  , ("hb_blog_media_url", String $ unPageMetaMediaUrl pageMeta)
  , ("hb_posteds", Array $ fromList (initPostedList postedElms))
  , ("hb_meta_description", String $ unPageMetaDescription pageMeta)
  , ("hb_meta_keywords", String $ unPageMetaKeywords pageMeta)
  , ("hb_meta_robots", String $ unPageMetaRobots pageMeta)
  , ("hb_tags", Array $ fromList (initMstTagList tags))
  , ("hb_meta_og_image", String $ unPageMetaOgImg pageMeta)
  , ("hb_meta_og_title", String $ unPageMetaOgTitle pageMeta)
  , ("hb_meta_og_url", String $ unPageMetaOgUrl pageMeta)
  , ("hb_meta_og_site_name", String $ unPageMetaOgSiteName pageMeta)
  , ("hb_meta_og_desc", String $ unPageMetaOgDesc pageMeta)
  , ("hb_meta_og_page_type", String $ unPageMetaOgPageType pageMeta)
  ]

setContextPost ::
  H.HashMap Text Value
  -> PageMeta
  -> [Post]
  -> H.HashMap Text Value
setContextPost renderInfo pageMeta posts =
  let (k1, v1) = ("hb_share_url", initPostShareUrl posts pageMeta)
      (k2, v2) = ("hb_posts", Array $ fromList (initPostList posts))
  in H.insert k2 v2 (H.insert k1 v1 renderInfo)

setContextFree ::
  H.HashMap Text Value
  -> PageMeta
  -> [Free]
  -> H.HashMap Text Value
setContextFree renderInfo pageMeta frees =
  let (k1, v1) = ("hb_share_url", initFreeShareUrl frees pageMeta)
      (k2, v2) = ("hb_posts", Array $ fromList (initFreeList frees))
  in H.insert k2 v2 (H.insert k1 v1 renderInfo)

initTagListShareUrl ::
  MstTag
  -> PageMeta
  -> Value
initTagListShareUrl msttag pageMeta =
  let tagId = unMstTagId msttag
      baseUrl = unPageMetaBlogUrl pageMeta
  in String $ rmSlash (baseUrl <> "/" <> toTagListUrlText tagId)
  
initPostShareUrl :: [Post] -> PageMeta -> Value
initPostShareUrl posts pageMeta =
  let post = headElm posts
      baseUrl = unPageMetaBlogUrl pageMeta
      pid = unPostId post
      slug = unPostSlug post
      urlpath = unPostUrlpath post
  in
    if length posts > 1 then
      String baseUrl
    else if all isJust [slug, urlpath] == True then
      String $ rmSlash (baseUrl <> "/" <> toPostSlugUrlText slug urlpath)
    else
      String $ rmSlash (baseUrl <> "/" <> toPostUrlText pid)

initFreeShareUrl :: [Free] -> PageMeta -> Value
initFreeShareUrl frees pageMeta =
  let free = headElm frees
      baseUrl = unPageMetaBlogUrl pageMeta
      fid = unFreeId free
      slug = unFreeSlug free
      urlpath = unFreeUrlpath free
  in
    if length frees > 1 then
      String baseUrl
    else if all isJust [slug, urlpath] == True then
      String $ rmSlash (baseUrl <> "/" <> toFreeSlugUrlText slug urlpath)
    else
      String $ rmSlash (baseUrl <> "/" <> toFreeUrlText fid)

headElm :: [a] -> a
headElm xs = if length xs >= 1 then (xs P.!! 0) else error "headElm: array is empty"

initPostList ::
  [Post]
  -> [Value]
initPostList posts = map (\post ->
  let (y, m, d, h, m', s) = toGrecoFullLocal $ fromJust
                              (utcToJpTime <$> unPostPublishDate post)
  in
    Object (
      GE.fromList [
          ("hb_body", String $ getPutHtml post)
          , ("hb_publish_date", Array $ fromList [
                  Number (fromIntegral y)
                , Number (fromIntegral m)
                , Number (fromIntegral d)
                , Number (fromIntegral h)
                , Number (fromIntegral m')
                , Number (fromIntegral s) ] )
          ]
      )) posts

initMstTagList ::
  [MstTag]
  -> [Value]
initMstTagList tags =
  map (\tag ->
    Object (         
      GE.fromList
        [
          ("hb_tag_id", Number $ (fromIntegral $ unMstTagId tag))
        , ("hb_tag_name", String $ unMstTagName tag)
        ]
      )
      ) tags

initPostedList ::
  [Posted]
  -> [Value]
initPostedList postedElms =
  map (\elm ->
    Object (
      GE.fromList [
          ("hb_posted_date", String $ unPostedDate elm)
        , ("hb_posted_title",
           Array $ fromList (
             map (\post ->
               Object (
                 GE.fromList
                 [
                      ("post_title", String $ unPostTitle post)
                    , ("post_id", Number $ (fromIntegral $ unPostId post))
                    , ("post_slug_url", String $ initSlugUrlPost post)
                 ]
               )
             ) (unPostedPosts elm))
          )
        ]
      )
    ) postedElms

toFramePageMeta ::
  BlogSetting
  -> Post
  -> Frame
  -> PageMeta
toFramePageMeta setting post frame = PageMeta {
    unPageMetaBlogName = unBlogSettingBlogName setting
  , unPageMetaTitle = unPostTitle post
  , unPageMetaBlogUrl = unBlogSettingBlogUrl setting
  , unPageMetaFrameCss = maybeToText $ unFrameCss frame
  , unPageMetaMediaUrl = unBlogSettingMediaUrl setting
  , unPageMetaCanonicalUrl = canonicalPath PTPost (unPostUrlpath post) (unPostSlug post)
  , unPageMetaDescription = maybeToText $ rmLfCr <$> unPostDescription post
  , unPageMetaKeywords = maybeToText $ unPostKeywords post
  , unPageMetaRobots = maybeToText $ unPostRobots post
  , unPageMetaOgImg = initOgImage setting (unPostOgImg post)
  , unPageMetaOgTitle = initOgTitle (unPostTitle post) (unPostOgTitle post)
  , unPageMetaOgUrl = initOgUrl PTPost
                                setting
                                (unPostId post)
                                (unPostSlug post)
                                (unPostUrlpath post)
                                (unPostOgUrl post)
  , unPageMetaOgSiteName = initOgSiteName setting (unPostOgSiteName post)
  , unPageMetaOgDesc = initOgDesc (unPostTitle post)
                                  (unPostDescription post)
                                  (unPostOgDesc post)
  , unPageMetaOgPageType = maybeToText $ unPostOgPageType post
  }

initOgDesc ::
  Text
  -> Maybe Text
  -> Maybe Text
  -> Text
initOgDesc title desc ogdesc =
  case ogdesc of
    Just ogdesc' -> ogdesc'
    Nothing ->
      case desc of
        Just desc' -> if T.length desc' > maxlen then cutdesc desc' else desc'
        Nothing -> if T.length title > maxlen then cutdesc title else title
  where cutdesc t = T.take maxlen t <> "..."
        maxlen = 30

initOgImage ::
  BlogSetting
  -> Maybe Text
  -> Text
initOgImage setting ogImgPath =
  case ogImgPath of
    Nothing -> ""
    Just imgpath  ->
      if unpack imgpath =~ ("https?://" :: String) then
        imgpath
      else
        blogPath (unBlogSettingMediaUrl setting) imgpath

initOgSiteName ::
    BlogSetting
  -> Maybe Text
  -> Text
initOgSiteName setting ogSiteName =
  case ogSiteName of
    Nothing -> unBlogSettingBlogName setting
    Just sitename -> sitename

initOgTitle ::
  Text
  -> Maybe Text
  -> Text
initOgTitle title ogTitle =
  case ogTitle of
    Nothing -> title
    Just title' -> title'

initOgUrl ::
  PageType
  -> BlogSetting
  -> Int64
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Text
initOgUrl pt setting fpid slug urlpath ogUrl =
  case ogUrl of
    Nothing ->
      if all isJust [urlpath, slug] == True then
        case pt of
          PTPost -> setBlogUrl $ toPostSlugUrlText slug urlpath
          PTFree -> setBlogUrl $ toFreeSlugUrlText slug urlpath
      else
        case pt of
          PTPost -> setBlogUrl $ toPostUrlText fpid
          PTFree -> setBlogUrl $ toFreeUrlText fpid
    Just ogurl -> ogurl
  where
    setBlogUrl url = blogPath (unBlogSettingBlogUrl setting) url

toTagContsFramePageMeta ::
  BlogSetting
  -> FreeFrame
  -> PageMeta
toTagContsFramePageMeta setting frame = PageMeta {
    unPageMetaBlogName = unBlogSettingBlogName setting
  , unPageMetaTitle = "タグ一覧"
  , unPageMetaBlogUrl = unBlogSettingBlogUrl setting
  , unPageMetaFrameCss = maybeToText $ unFreeFrameCss frame
  , unPageMetaCanonicalUrl = ""
  , unPageMetaMediaUrl = unBlogSettingMediaUrl setting
  , unPageMetaDescription = ""
  , unPageMetaKeywords = ""
  , unPageMetaRobots = "index,follow"
  , unPageMetaOgImg = ""
  , unPageMetaOgTitle = "タグ一覧"
  , unPageMetaOgUrl = ""
  , unPageMetaOgSiteName = ""
  , unPageMetaOgDesc = ""
  , unPageMetaOgPageType = ""
  }

toFreeFramePageMeta ::
  BlogSetting
  -> Free
  -> FreeFrame
  -> PageMeta
toFreeFramePageMeta setting free frame = PageMeta {
    unPageMetaBlogName = unBlogSettingBlogName setting
  , unPageMetaTitle = unFreeTitle free
  , unPageMetaBlogUrl = unBlogSettingBlogUrl setting
  , unPageMetaFrameCss = maybeToText $ unFreeFrameCss frame
  , unPageMetaCanonicalUrl = canonicalPath PTFree (unFreeUrlpath free) (unFreeSlug free)
  , unPageMetaMediaUrl = unBlogSettingMediaUrl setting
  , unPageMetaDescription = maybeToText $ rmLfCr <$> unFreeDescription free
  , unPageMetaKeywords = maybeToText $ unFreeKeywords free
  , unPageMetaRobots = maybeToText $ unFreeRobots free
  , unPageMetaOgImg = initOgImage setting (unFreeOgImg free)
  , unPageMetaOgTitle = initOgTitle (unFreeTitle free) (unFreeOgTitle free)
  , unPageMetaOgUrl = initOgUrl PTFree
                                setting
                                (unFreeId free)
                                (unFreeSlug free)
                                (unFreeUrlpath free)
                                (unFreeOgUrl free)
  , unPageMetaOgSiteName = initOgSiteName setting (unFreeOgSiteName free)
  , unPageMetaOgDesc = initOgDesc (unFreeTitle free)
                                  (unFreeDescription free)
                                  (unFreeOgDesc free)
  , unPageMetaOgPageType = maybeToText $ unFreeOgPageType free
  }

getPutHtml ::
  Post
  -> Text
getPutHtml post =
  if unPostInputType post == (fromEnum ContTypeMarkdown) then
    maybeToText $ unPostHtml post
  else
    unPostContent post

getPutHtmlFree ::
  Free
  -> Text
getPutHtmlFree free =
  if unFreeInputType free == (fromEnum ContTypeMarkdown) then
    maybeToText $ unFreeHtml free
  else
    unFreeContent free

toPostToPosted ::
  [Post]
  -> [Posted]
toPostToPosted xs = toPostToPosted' pxs
  where pxs = map (\p ->
                     Posted (
                       dateFormatYM (fromJust $ unPostPublishDate p)) [p]
                  ) xs

toPostToPosted' ::
  [Posted]
  -> [Posted]
toPostToPosted' (p1:p2:xs) =
  if unPostedDate p1 == unPostedDate p2 then
    toPostToPosted' ((aggrPosted p1 p2):xs)
  else
    [p1] ++ toPostToPosted' (p2:xs)
toPostToPosted' [p] = [p]
toPostToPosted' [] = []

aggrPosted ::
  Posted
  -> Posted
  -> Posted
aggrPosted p1 p2 = Posted (unPostedDate p1) (unPostedPosts p1 ++ unPostedPosts p2)

zoneTimeToValue ::
  (Integer, Int, Int, Int, Int, Int)
  -> Value
zoneTimeToValue (y, m, d, h, m', s) =
  Array $ fromList [
      Number (fromIntegral y)
    , Number (fromIntegral m)
    , Number (fromIntegral d)
    , Number (fromIntegral h)
    , Number (fromIntegral m')
    , Number (fromIntegral s) ]

createPrevBlogContents ::
  Text
  -> Post
  -> Handler (HResult Html)
createPrevBlogContents setId post = do
  setting <- getBlogSetting setId
  frame <- eitherToMaybe =<< readPostFrame
  posted <- eitherToList =<< getPublishPostedList
  tags <- eitherToList =<< getTagList
  case (frame /= Nothing) of
    False -> do
      $(logInfo) $ "createPrevBlogContents': frame or posts undefined"
      return $ Left ErrPostUnitialize
    True -> do
      html <- renderBlogContents setting (fromJust frame) [post] posted tags
      return html

createPrevBlogFreeContents ::
  Text
  -> Free
  -> Handler (HResult Html)
createPrevBlogFreeContents setId free = do
  setting <- getBlogSetting setId
  frame <- eitherToMaybe =<< readFreeFrame
  posted <- eitherToList =<< getPublishPostedList
  tags <- eitherToList =<< getTagList
  case (frame /= Nothing) of
    False -> return $ Left ErrFreeUnitialize
    True -> do
      html <- renderBlogFreeContents setting (fromJust frame) [free] posted tags
      return html

initSlugUrlPost :: Post -> Text
initSlugUrlPost post =
  if isJust (unPostSlug post) && isJust (unPostUrlpath post) then
    toPostSlugUrlText (unPostSlug post) (unPostUrlpath post)
  else
    ""
