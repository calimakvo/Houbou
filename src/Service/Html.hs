{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Html (
      createBlogContents
    , createBlogFreeContents
    , createTagListContents
    , createCateListContents
    , createPrevBlogContents
    , createPrevBlogFreeContents
  ) where

import Import
import qualified Prelude as P
import qualified Data.Scientific as S
import Data.Maybe
import qualified Data.Text as T
import Text.Regex.PCRE
import Text.Blaze.Html
import qualified Data.HashMap.Strict as H
import DataTypes.HoubouType
import UrlParam.PostId
import UrlParam.FreeId
import UrlParam.TagId
import UrlParam.CateId
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
import Service.Category
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
  postinf <- initPostInfo posts
  posted <- eitherToList =<< getPublishPostedList 0 100
  tags <- eitherToList =<< getTagList 100
  case (frame /= Nothing && null posts == False) of
    False -> do
      $(logInfo) $ "createBlogContents': frame or posts undefined"
      return $ Left ErrPostUnitialize
    True -> do
      html <- renderBlogContents setting (fromJust frame) postinf posted tags
      return html

renderBlogContents ::
  BlogSetting
  -> Frame
  -> [PostInf]
  -> [Post]
  -> [MstTag]
  -> Handler (HResult Html)
renderBlogContents setting frame postinf posted tags = do
  let pageMeta = toFramePageMeta setting (postinf P.!! 0) frame
      postedElms = toPostToPosted posted
      renderInfo' = initContextPost pageMeta postedElms tags
  pinf <- liftIO $ preRenderPost postinf renderInfo'
  template <- liftIO $ createTemplate Nothing (unpack $ internalComp frame)
  case template of
    Left err -> do
      $(logInfo) $ "renderBlogContents': template parse error = " <> toText(err)
      return $ Left ErrTemplateUnInit
    Right template' -> do
      let renderInfo = setContextPost renderInfo' pageMeta pinf
      return $ Right $ preEscapedToHtml (render template' renderInfo)

preRenderPost ::
  [PostInf]
  -> HashMap G.VarName Value
  -> IO [PostInf]
preRenderPost postinf renderInfo = do
  pinf <- mapM (\(PostInf p c) -> do
      template <- createTemplate Nothing (unpack $ getPutHtml p)
      case template of
        Left err -> do
          error $ "preRenderPost: post template parse error post_id=" ++
                    show(unPostId p) ++ " err=" ++ show(err)
        Right template' -> do
          return $ PostInf
            { unPostInfPost = p { unPostHtml = Just (render template' renderInfo) }
            , unPostInfCate = c
            }) postinf
  return pinf

createBlogFreeContents ::
  Text
  -> FreeId
  -> Handler (HResult Html)
createBlogFreeContents setId freeId = do
  setting <- getBlogSetting setId
  frame <- eitherToMaybe =<< readFreeFrame
  frees <- eitherToList =<< (getPublishFreeFromId setting freeId)
  freeinf <- initFreeInfo frees
  posted <- eitherToList =<< getPublishPostedList 0 100
  tags <- eitherToList =<< getTagList 100
  case (frame /= Nothing && null frees == False) of
    False -> return $ Left ErrFreeUnitialize
    True -> do
      html <- renderBlogFreeContents setting (fromJust frame) freeinf posted tags
      return html

renderBlogFreeContents ::
  BlogSetting
  -> FreeFrame
  -> [FreeInf]
  -> [Post]
  -> [MstTag]
  -> Handler (HResult Html)
renderBlogFreeContents setting frame freeinf posted tags = do
  let pageMeta = toFreeFramePageMeta setting (freeinf P.!! 0) frame
      postedElms = toPostToPosted posted
      renderInfo' = initContextFree pageMeta postedElms tags
  finf <- liftIO $ preRenderFree freeinf renderInfo'
  template <- liftIO $ createTemplate Nothing (unpack $ internalFreeComp frame)
  case template of
    Left err -> do
      $(logInfo) $ "createBlogFreeContents': template parse error = " <> toText(err)
      return $ Left ErrTemplateUnInit
    Right template' -> do
      let renderInfo = setContextFree renderInfo' pageMeta finf
      return $ Right $ preEscapedToHtml (render template' renderInfo)

preRenderFree ::
  [FreeInf]
  -> HashMap G.VarName Value
  -> IO [FreeInf]
preRenderFree freeinf renderInfo = do
  finf <- mapM (\(FreeInf f c) -> do
      template <- createTemplate Nothing (unpack $ getPutHtmlFree f)
      case template of
        Left err -> do
          error $ "preRenderFree: free template parse error free_id="
                    ++ show(unFreeId f)
                    ++ " err=" ++ show(err)
        Right template' -> do
          return $ FreeInf
            { unFreeInfFree = f { unFreeHtml = Just (render template' renderInfo) }
            , unFreeInfCate = c
            }) freeinf
  return finf

createTagListContents ::
  Text
  -> TagId
  -> Handler (HResult Html)
createTagListContents setId tagId = do
  setting <- getBlogSetting setId
  frame <- eitherToMaybe =<< readFreeFrame
  msttag <- eitherToMaybe =<< getMstTagFromId tagId
  tagconts <- eitherToList =<< getTagContents tagId
  posted <- eitherToList =<< getPublishPostedList 0 100
  tags <- eitherToList =<< getTagList 100
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
  -> [PfContent TagInfo]
  -> [Post]
  -> [MstTag]
  -> MstTag
  -> Handler (HResult Html)
renderTagListContents' setting frame tagconts posted tags msttag = do
  template <- liftIO $ createTemplate Nothing (unpack $ internalFreeComp frame)
  case template of
    Left err -> do
      $(logInfo) $ "renderTagListContents': template parse error = " <> toText(err)
      return $ Left ErrTemplateUnInit
    Right template' -> do
      return $ Right $ preEscapedToHtml
        (render template' (initContextTagCont pageMeta
                           tagconts postedElms tags msttag))
  where
    pageMeta = toContsFramePageMeta "タグ一覧" setting frame
    postedElms = toPostToPosted posted

createCateListContents ::
  Text
  -> CateId
  -> Handler (HResult Html)
createCateListContents setId cateId = do
  setting <- getBlogSetting setId
  frame <- eitherToMaybe =<< readFreeFrame
  cate <- eitherToMaybe =<< getCategoryFromCateId cateId
  cateconts <- eitherToList =<< getCateContents cateId
  posted <- eitherToList =<< getPublishPostedList 0 100
  tags <- eitherToList =<< getTagList 100
  case (frame /= Nothing && isJust cate == True) of
    False -> do
      $(logInfo) $ "createCateListContents: cate data invalid."
      return $ Left ErrTagContUnitialize
    True -> do
      html <- renderCateListContents' setting (fromJust frame) cateconts
                                       posted tags (fromJust cate)
      return html

renderCateListContents' ::
  BlogSetting
  -> FreeFrame
  -> [PfContent CateInfo]
  -> [Post]
  -> [MstTag]
  -> Cate
  -> Handler (HResult Html)
renderCateListContents' setting frame cateconts posted tags cate = do
  template <- liftIO $ createTemplate Nothing (unpack $ internalFreeComp frame)
  case template of
    Left err -> do
      $(logInfo) $ "renderCateListContents': template parse error = " <> toText(err)
      return $ Left ErrTemplateUnInit
    Right template' -> do
      cs <- getCategoryBreadCrumbs (int64ToInt $ unCateId cate)
      let baseUrl = unBlogSettingBlogUrl setting
          bclink = catesToTextBreadCrumbs baseUrl cs
      return $ Right $ preEscapedToHtml
        (render template' (initContextCateCont pageMeta
                           cateconts postedElms tags cate bclink))
  where
    pageMeta = toContsFramePageMeta "カテゴリ一覧" setting frame
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
  -> [PfContent TagInfo]
  -> [Posted]
  -> [MstTag]
  -> MstTag
  -> H.HashMap Text Value
initContextTagCont pageMeta tagcont postedElms tags msttag = H.fromList [
    ("hb_blog_name", String $ unPageMetaBlogName pageMeta)
  , ("hb_blog_title", String $ unPageMetaTitle pageMeta)
  , ("hb_search_tag", String $ unMstTagName msttag)
  , ("hb_blog_url", String $ unPageMetaBlogUrl pageMeta)
  , ("hb_blog_author", String $ unPageMetaBlogAuthor pageMeta)
  , ("hb_blog_desc", String $ unPageMetaBlogDesc pageMeta)
  , ("hb_blog_css", String $ unPageMetaFrameCss pageMeta)
  , ("hb_share_url", initTagListShareUrl msttag pageMeta)
  , ("hb_blog_media_url", String $ unPageMetaMediaUrl pageMeta)
  , ("hb_tagconts", Array $ fromList (initTagContList tagcont))
  , ("hb_posteds", Array $ fromList (initPostedList postedElms))
  , ("hb_tags", Array $ fromList (initMstTagList tags)) ]

initContextCateCont ::
  PageMeta
  -> [PfContent CateInfo]
  -> [Posted]
  -> [MstTag]
  -> Cate
  -> Text
  -> H.HashMap Text Value
initContextCateCont pageMeta catecont postedElms tags cate bclink = H.fromList [
    ("hb_blog_name", String $ unPageMetaBlogName pageMeta)
  , ("hb_blog_title", String $ unPageMetaTitle pageMeta)
  , ("hb_search_cate", String $ unCateName cate)
  , ("hb_cate_breadcrumbs", String $ bclink)
  , ("hb_blog_url", String $ unPageMetaBlogUrl pageMeta)
  , ("hb_blog_author", String $ unPageMetaBlogAuthor pageMeta)
  , ("hb_blog_desc", String $ unPageMetaBlogDesc pageMeta)
  , ("hb_blog_css", String $ unPageMetaFrameCss pageMeta)
  , ("hb_share_url", initCateListShareUrl cate pageMeta)
  , ("hb_blog_media_url", String $ unPageMetaMediaUrl pageMeta)
  , ("hb_cateconts", Array $ fromList (initCateContList catecont))
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
  , ("hb_blog_author", String $ unPageMetaBlogAuthor pageMeta)
  , ("hb_blog_desc", String $ unPageMetaBlogDesc pageMeta)
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
  [PfContent TagInfo]
  -> [Value]
initTagContList tagconts =
  map (\tagcont ->
         case tagcont of
           ContPost taginfo -> createObjectTagPost taginfo
           ContFree taginfo -> createObjectTagFree taginfo
      ) tagconts
  where
    pubdate ut = toGrecoFullLocal (utcToJpTime ut)
    createObjectTagPost :: TagInfo -> Value
    createObjectTagPost tinfo =
      Object (
        GE.fromList [
                ("type", Number $ fromIntegral (fromEnum TypePost))
              , ("id", Number (fromIntegral $ unTagInfoId tinfo))
              , ("title", String $ unTagInfoTitle tinfo)
              , ("posted", zoneTimeToValue (pubdate (unTagInfoPosted tinfo)))
              ] )
    createObjectTagFree :: TagInfo -> Value
    createObjectTagFree tinfo =
      Object (
        GE.fromList [
                 ("type", Number $ fromIntegral (fromEnum TypeFree))
               , ("id", Number (fromIntegral $ unTagInfoId tinfo))
               , ("title", String $ unTagInfoTitle tinfo)
               , ("posted", zoneTimeToValue (pubdate (unTagInfoPosted tinfo)))
               ] )

initCateContList ::
  [PfContent CateInfo]
  -> [Value]
initCateContList cateconts =
  map (\catecont ->
         case catecont of
           ContPost cateinfo -> createObjectCatePost cateinfo
           ContFree cateinfo -> createObjectCateFree cateinfo
      ) cateconts
  where
    pubdate ut = toGrecoFullLocal (utcToJpTime ut)
    createObjectCatePost :: CateInfo -> Value
    createObjectCatePost cinfo =
      Object (
        GE.fromList [
                ("type", Number $ fromIntegral (fromEnum TypePost))
              , ("id", Number (fromIntegral $ unCateInfoPfId cinfo))
              , ("title", String $ unCateInfoTitle cinfo)
              , ("posted", zoneTimeToValue (pubdate (unCateInfoPosted cinfo)))
              ] )
    createObjectCateFree :: CateInfo -> Value
    createObjectCateFree cinfo =
      Object (
        GE.fromList [
                 ("type", Number $ fromIntegral (fromEnum TypeFree))
               , ("id", Number (fromIntegral $ unCateInfoPfId cinfo))
               , ("title", String $ unCateInfoTitle cinfo)
               , ("posted", zoneTimeToValue (pubdate (unCateInfoPosted cinfo)))
               ] )

initFreeList ::
  [FreeInf]
  -> [Value]
initFreeList freeinf =
  map (\(FreeInf free cate) ->
          let d = toGrecoFullLocal $ fromJust (utcToJpTime <$> unFreePublishDate free)
          in
            Object (
              GE.fromList [
                    ("hb_body", String $ getPutHtmlFree free)
                  , ( "hb_category_list", Array $ fromList (
                        map (\c ->
                               Object ( GE.fromList
                                        [ ("hb_cate_id", Number $ S.scientific (fromIntegral $ unCateId c) 0)
                                        , ("hb_cate_name", String $ unCateName c)
                                        , ("hb_cate_pos", Number $ S.scientific (fromIntegral $ unCatePos c) 0)
                                        ])) cate
                        )
                    )
                  , ("hb_css",  String $ (maybeToText $ unFreeCss free))
                  , ("hb_publish_date",  zoneTimeToValue d)
                  ]
              )) freeinf

initContextPost ::
  PageMeta
  -> [Posted]
  -> [MstTag]
  -> H.HashMap Text Value
initContextPost pageMeta postedElms tags = H.fromList [
    ("hb_blog_name", String $ unPageMetaBlogName pageMeta)
  , ("hb_blog_title", String $ unPageMetaTitle pageMeta)
  , ("hb_blog_url", String $ unPageMetaBlogUrl pageMeta)
  , ("hb_blog_author", String $ unPageMetaBlogAuthor pageMeta)
  , ("hb_blog_desc", String $ unPageMetaBlogDesc pageMeta)
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
  -> [PostInf]
  -> H.HashMap Text Value
setContextPost renderInfo pageMeta postinf =
  let (k1, v1) = ("hb_share_url", initPostShareUrl postinf pageMeta)
      (k2, v2) = ("hb_posts", Array $ fromList (initPostList postinf))
  in H.insert k2 v2 (H.insert k1 v1 renderInfo)

setContextFree ::
  H.HashMap Text Value
  -> PageMeta
  -> [FreeInf]
  -> H.HashMap Text Value
setContextFree renderInfo pageMeta freeinf =
  let (k1, v1) = ("hb_share_url", initFreeShareUrl freeinf pageMeta)
      (k2, v2) = ("hb_posts", Array $ fromList (initFreeList freeinf))
  in H.insert k2 v2 (H.insert k1 v1 renderInfo)

initTagListShareUrl ::
  MstTag
  -> PageMeta
  -> Value
initTagListShareUrl msttag pageMeta =
  let tagId = unMstTagId msttag
      baseUrl = unPageMetaBlogUrl pageMeta
  in String $ rmSlash (baseUrl <> "/" <> toTagListUrlText tagId)

initCateListShareUrl ::
  Cate
  -> PageMeta
  -> Value
initCateListShareUrl cate pageMeta =
  let cateId = unCateId cate
      baseUrl = unPageMetaBlogUrl pageMeta
  in String $ rmSlash (baseUrl <> "/" <> toCateListUrlText cateId)
  
initPostShareUrl :: [PostInf] -> PageMeta -> Value
initPostShareUrl postinf pageMeta =
  let PostInf post _ = headElm postinf
      baseUrl = unPageMetaBlogUrl pageMeta
      pid = unPostId post
      slug = unPostSlug post
      urlpath = unPostUrlpath post
  in
    if length postinf > 1 then
      String baseUrl
    else if all isJust [slug, urlpath] == True then
      String $ rmSlash (baseUrl <> "/" <> toPostSlugUrlText slug urlpath)
    else
      String $ rmSlash (baseUrl <> "/" <> toPostUrlText pid)

initFreeShareUrl :: [FreeInf] -> PageMeta -> Value
initFreeShareUrl freeinf pageMeta =
  let FreeInf free _ = headElm freeinf
      baseUrl = unPageMetaBlogUrl pageMeta
      fid = unFreeId free
      slug = unFreeSlug free
      urlpath = unFreeUrlpath free
  in
    if length freeinf > 1 then
      String baseUrl
    else if all isJust [slug, urlpath] == True then
      String $ rmSlash (baseUrl <> "/" <> toFreeSlugUrlText slug urlpath)
    else
      String $ rmSlash (baseUrl <> "/" <> toFreeUrlText fid)

headElm :: [a] -> a
headElm xs = if length xs >= 1 then (xs P.!! 0) else error "headElm: array is empty"

initPostList ::
  [PostInf]
  -> [Value]
initPostList postinf = map (\(PostInf post cate) ->
  let (y, m, d, h, m', s) = toGrecoFullLocal $ fromJust
                              (utcToJpTime <$> unPostPublishDate post)
  in
    Object (
      GE.fromList [
            ( "hb_body", String $ getPutHtml post)
          , ( "hb_category_list", Array $ fromList (
                 map (\c ->
                         Object ( GE.fromList
                         [ ("hb_cate_id", Number $ S.scientific (fromIntegral $ unCateId c) 0)
                         , ("hb_cate_name", String $ unCateName c)
                         , ("hb_cate_pos", Number $ S.scientific (fromIntegral $ unCatePos c) 0)
                         ])) cate
                 )
            )
          , ( "hb_publish_date", Array $ fromList [
                  Number (fromIntegral y)
                , Number (fromIntegral m)
                , Number (fromIntegral d)
                , Number (fromIntegral h)
                , Number (fromIntegral m')
                , Number (fromIntegral s) ] )
          ]
      )) postinf

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
  -> PostInf
  -> Frame
  -> PageMeta
toFramePageMeta setting (PostInf post _) frame = PageMeta {
    unPageMetaBlogName = unBlogSettingBlogName setting
  , unPageMetaTitle = unPostTitle post
  , unPageMetaBlogUrl = unBlogSettingBlogUrl setting
  , unPageMetaBlogAuthor = unBlogSettingBlogAuthor setting
  , unPageMetaBlogDesc = unBlogSettingBlogDesc setting
  , unPageMetaFrameCss = maybeToText $ unFrameCss frame
  , unPageMetaMediaUrl = unBlogSettingMediaUrl setting
  , unPageMetaCanonicalUrl = canonicalPath TypePost (unPostUrlpath post) (unPostSlug post)
  , unPageMetaDescription = maybeToText $ rmLfCr <$> unPostDescription post
  , unPageMetaKeywords = maybeToText $ unPostKeywords post
  , unPageMetaRobots = maybeToText $ unPostRobots post
  , unPageMetaOgImg = initOgImage setting (unPostOgImg post)
  , unPageMetaOgTitle = initOgTitle (unPostTitle post) (unPostOgTitle post)
  , unPageMetaOgUrl = initOgUrl TypePost
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
          TypePost -> setBlogUrl $ toPostSlugUrlText slug urlpath
          TypeFree -> setBlogUrl $ toFreeSlugUrlText slug urlpath
          _ -> error "unreachable code"
      else
        case pt of
          TypePost -> setBlogUrl $ toPostUrlText fpid
          TypeFree -> setBlogUrl $ toFreeUrlText fpid
          _ -> error "unreachable code"
    Just ogurl -> ogurl
  where
    setBlogUrl url = blogPath (unBlogSettingBlogUrl setting) url

toContsFramePageMeta ::
     Text
  -> BlogSetting
  -> FreeFrame
  -> PageMeta
toContsFramePageMeta title setting frame = PageMeta {
    unPageMetaBlogName = unBlogSettingBlogName setting
  , unPageMetaTitle = title
  , unPageMetaBlogUrl = unBlogSettingBlogUrl setting
  , unPageMetaBlogAuthor = unBlogSettingBlogAuthor setting
  , unPageMetaBlogDesc = unBlogSettingBlogDesc setting
  , unPageMetaFrameCss = maybeToText $ unFreeFrameCss frame
  , unPageMetaCanonicalUrl = ""
  , unPageMetaMediaUrl = unBlogSettingMediaUrl setting
  , unPageMetaDescription = ""
  , unPageMetaKeywords = ""
  , unPageMetaRobots = "index,follow"
  , unPageMetaOgImg = ""
  , unPageMetaOgTitle = title
  , unPageMetaOgUrl = ""
  , unPageMetaOgSiteName = ""
  , unPageMetaOgDesc = ""
  , unPageMetaOgPageType = ""
  }

toFreeFramePageMeta ::
  BlogSetting
  -> FreeInf
  -> FreeFrame
  -> PageMeta
toFreeFramePageMeta setting (FreeInf free _) frame = PageMeta {
    unPageMetaBlogName = unBlogSettingBlogName setting
  , unPageMetaTitle = unFreeTitle free
  , unPageMetaBlogUrl = unBlogSettingBlogUrl setting
  , unPageMetaBlogAuthor = unBlogSettingBlogAuthor setting
  , unPageMetaBlogDesc = unBlogSettingBlogDesc setting
  , unPageMetaFrameCss = maybeToText $ unFreeFrameCss frame
  , unPageMetaCanonicalUrl = canonicalPath TypeFree (unFreeUrlpath free) (unFreeSlug free)
  , unPageMetaMediaUrl = unBlogSettingMediaUrl setting
  , unPageMetaDescription = maybeToText $ rmLfCr <$> unFreeDescription free
  , unPageMetaKeywords = maybeToText $ unFreeKeywords free
  , unPageMetaRobots = maybeToText $ unFreeRobots free
  , unPageMetaOgImg = initOgImage setting (unFreeOgImg free)
  , unPageMetaOgTitle = initOgTitle (unFreeTitle free) (unFreeOgTitle free)
  , unPageMetaOgUrl = initOgUrl TypeFree
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
  -> PostInf
  -> Handler (HResult Html)
createPrevBlogContents setId postinf = do
  setting <- getBlogSetting setId
  frame <- eitherToMaybe =<< readPostFrame
  posted <- eitherToList =<< getPublishPostedList 0 100
  tags <- eitherToList =<< getTagList 100
  case (frame /= Nothing) of
    False -> do
      $(logInfo) $ "createPrevBlogContents': frame or posts undefined"
      return $ Left ErrPostUnitialize
    True -> do
      html <- renderBlogContents setting (fromJust frame) [postinf] posted tags
      return html

createPrevBlogFreeContents ::
  Text
  -> FreeInf
  -> Handler (HResult Html)
createPrevBlogFreeContents setId freeinf = do
  setting <- getBlogSetting setId
  frame <- eitherToMaybe =<< readFreeFrame
  posted <- eitherToList =<< getPublishPostedList 0 100
  tags <- eitherToList =<< getTagList 100
  case (frame /= Nothing) of
    False -> return $ Left ErrFreeUnitialize
    True -> do
      html <- renderBlogFreeContents setting (fromJust frame) [freeinf] posted tags
      return html

initSlugUrlPost :: Post -> Text
initSlugUrlPost post =
  if isJust (unPostSlug post) && isJust (unPostUrlpath post) then
    toPostSlugUrlText (unPostSlug post) (unPostUrlpath post)
  else
    ""
