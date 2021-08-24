{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Category
  ( getCategory
  , getCategories
  , registerCategory
  , updateCatefory
  , updateCategoryList
  , checkCategoryUse
  , deleteCatefory
  , getCategoryRel
  , getCategoryFromId
  , getCategoryFromCateId
  , getCateContents
  , getCategoryBreadCrumbs
  , recursiveCate
  , recursiveCateEntity
  ) where

import Import
import qualified Data.Maybe as M
import qualified Data.Map.Ordered as O
import Database.Persist.Sql (BackendKey(..))
import qualified Database.Esqueleto as E
import DataTypes.HoubouType
import UrlParam.CateId
import Libs.Common
import Service.Common

getCategory ::
  Int64
  -> Handler (HResult Cate)
getCategory cid = runDB $ do
  cate <- get $ toTblCategoryKey cid
  case cate of
    Just c -> return $ Right $ toCateTbl cid c
    _ -> return $ Left ErrRecNotFound

getCategoryPid :: MonadIO m =>
  Maybe Int
  -> ReaderT SqlBackend m [Cate]
getCategoryPid cateId = do
  list <- selectList [ TblCategoryParentId ==. cateId ]
                     [ Asc TblCategoryDispOrder ]
  return $ map toCate list

getCategoryFromCateId ::
  CateId
  -> Handler (HResult Cate)
getCategoryFromCateId cateId = runDB $ do
  let cateid = M.fromJust $ _unUrlCateId cateId
  res <- get $ toTblCategoryKey cateid
  case res of
    Just c -> return $ Right $ toCateTbl cateid c
    _ -> return $ Left ErrRecNotFound

getCategoryFromId ::
  Int
  -> Handler (HResult Cate)
getCategoryFromId cid = runDB $ do
  res <- get $ toTblCategoryKey (intToInt64 cid)
  case res of
    Just c -> return $ Right $ toCateTbl (intToInt64 cid) c
    _ -> return $ Left ErrRecNotFound

getCategoryRel ::
  Handler (O.OMap Int [Cate])
getCategoryRel = do
  pcs <- getCategories
  let cs = getCategoryList pcs
  m <- chileCateMap O.empty cs
  return m

getCategoryList :: [Cate] -> [Cate]
getCategoryList xs =
  case xs of
    (c:cs) -> [c] ++ getCategoryList (unCateList c) ++ getCategoryList cs
    [] -> []

chileCateMap ::
  (O.OMap Int [Cate])
  -> [Cate]
  -> Handler (O.OMap Int [Cate])
chileCateMap m pcs = do
  case pcs of
    (c:cs) ->
      if unCatePid c == Nothing then
        chileCateMap (((int64ToInt $ unCateId c), [c]) O.|< m) cs
      else
        case O.lookup (M.fromJust $ unCatePid c) m of
          Nothing -> error "chileCateMap: Category data is not sorted."
          Just ics -> chileCateMap (((int64ToInt $ unCateId c), (ics ++ [c])) O.|<  m) cs
    [] -> return m

getCategoryBreadCrumbs ::
  Int
  -> Handler [Cate]
getCategoryBreadCrumbs cid = runDB $ do
  res <- liftHandler $ getCategoryFromId cid
  case res of
    Right cate -> do
      case unCatePid cate of
        Just pid -> do
          cate' <- liftHandler $ getCategoryBreadCrumbs pid
          return $ cate'++ [cate]
        Nothing -> return [cate]
    Left _ -> return []

getCategories :: Handler [Cate]
getCategories = runDB $ do
  pcs <- getCategoryPid Nothing
  cs <- mapM recursiveCate pcs
  return cs

recursiveCate :: MonadIO m =>
  Cate
  -> ReaderT SqlBackend m Cate
recursiveCate c = do
  chd <- getCategoryPid (Just $ int64ToInt $ unCateId c)
  case chd of
    (_:xs) -> do
      xs' <- mapM recursiveCate xs
      return $ c { unCateList = xs' }
    [] -> return  $ c { unCateList = [] }

registerCategory ::
  CateSetting
  -> Handler (HResult Int64)
registerCategory cateSet = runDB $ do
  let pId = unCateSettingParentId cateSet
  dispOrder <- liftHandler $ checkCateDispOrder cateSet
  now <- liftIO getTm
  TblCategoryKey (SqlBackendKey cateId) <- insert $
    TblCategory
      { tblCategoryName = unCateSettingName cateSet
      , tblCategoryParentId = if pId > 0 then Just pId else Nothing
      , tblCategoryDispOrder = dispOrder + 1
      , tblCategoryCreateTime = now
      , tblCategoryUpdateTime = now
      , tblCategoryVersion = 1
      }
  return $ Right cateId

updateCatefory ::
  CateSetting
  -> Handler (HResult Int64)
updateCatefory cateSet = runDB $ do
  let cateKey = toTblCategoryKey $ unCateSettingId cateSet
  curRec <- get cateKey
  res <- case curRec of
    Nothing -> return $ Left ErrRecNotFound
    Just record -> do
      let pver = unCateSettingVersion cateSet
          dver = tblCategoryVersion record
      case checkVersion dver pver of
        False -> return $ Left ErrRecVersion
        True -> do
          now <- liftIO getTm
          update cateKey [
              TblCategoryName =. unCateSettingName cateSet
            , TblCategoryUpdateTime =. now
            , TblCategoryVersion +=. 1
            ]
          return $ Right (unCateSettingId cateSet)
  return res

updateCategoryList :: [Cate] -> Handler (HResult Bool)
updateCategoryList cs = runDB $ do
  verchk <- cateVersionCheck cs
  case verchk of
    False -> return $ Left ErrRecVersion
    True -> do
      _ <- updateCategoryList' cs
      return $ Right True

updateCategoryList' :: MonadIO m =>
  [Cate]
  -> ReaderT SqlBackend m Bool
updateCategoryList' cs = do
  let idxcs = zip [1..length(cs)] cs
  _ <- mapM (\(dispOrder, c) -> do
              now <- liftIO getTm
              let cateKey = toTblCategoryKey $ unCateId c
              update cateKey [
                  TblCategoryName =. unCateName c
                , TblCategoryDispOrder =. dispOrder
                , TblCategoryUpdateTime =. now
                , TblCategoryVersion +=. 1
                ]
              updateCategoryList' (unCateList c)
            ) idxcs
  return True

checkCateDispOrder ::
  CateSetting
  -> Handler Int
checkCateDispOrder cateSet = runDB $ do
  let pCate =
        if unCateSettingParentId cateSet == 0
        then Nothing
        else (Just $ unCateSettingParentId cateSet)
  cates <- selectList [ TblCategoryParentId ==. pCate ] []
  return $ length cates

cateVersionCheck :: MonadIO m =>
  [Cate]
  -> ReaderT SqlBackend m Bool
cateVersionCheck (c:cs) = do
  let cateKey = toTblCategoryKey $ unCateId c
  res <- get cateKey
  case res of
    Nothing -> do
      return False
    Just c' -> do
      if unCateVer c == tblCategoryVersion c' then
        cateVersionCheck cs
      else
        return False
cateVersionCheck [] = return True

checkCategoryUse ::
  Int64
  -> Handler CateUseStatus
checkCategoryUse cateId = runDB $ do
  let cateKey = toTblCategoryKey cateId
  r1 <- selectFirst [ TblPostCateId ==. Just cateKey ] []
  case r1 of
    Just _ -> return CateUsePost
    Nothing -> do
      r2 <- selectFirst [ TblFreeCateId ==. Just cateKey ] []
      case r2 of
        Just _ -> return CateUseFree
        Nothing -> do
          r3 <- selectFirst [ TblCategoryParentId ==. Just (int64ToInt cateId) ] []
          case r3 of
            Just _ -> return CateHasChild
            Nothing -> return CateNotUse

deleteCatefory ::
  Int64
  -> Int
  -> Handler (HResult Bool)
deleteCatefory cateId ver = runDB $ do
  let cateKey = toTblCategoryKey cateId
  curRec <- get cateKey
  res <- case curRec of
    Nothing -> return $ Left ErrRecNotFound
    Just record -> do
      let dver = tblCategoryVersion record
          pCate = tblCategoryParentId record
      case checkVersion dver ver of
        False -> return $ Left ErrRecVersion
        True -> do
          delete cateKey
          cates <- selectList [ TblCategoryParentId ==. pCate ] [Asc TblCategoryDispOrder]
          mapM_ (\(dispOrder, Entity key _) -> do
                    now <- liftIO getTm
                    update key [
                        TblCategoryDispOrder =. dispOrder
                      , TblCategoryUpdateTime =. now
                      , TblCategoryVersion +=. 1
                      ]
                    ) $ zip [1..length cates] cates
          return $ Right True
  return res

getCateContents ::
  CateId
  -> Handler (HResult [PfContent CateInfo])
getCateContents cateId = runDB $ do
  let cateid = M.fromJust $ _unUrlCateId cateId
      sql = "SELECT pfid, rectype, cate_id, title, publish_date FROM " <>
            "((SELECT id AS pfid, ? AS rectype, t1.cate_id, t1.title, t1.publish_date FROM tbl_post AS t1 " <>
            "WHERE t1.status=? AND t1.publish_date IS NOT NULL) " <>
            "UNION" <>
            "(SELECT id AS pfid, ? AS rectype, t2.cate_id, t2.title, t2.publish_date FROM tbl_free AS t2 " <>
            "WHERE t2.status=? AND t2.publish_date IS NOT NULL)) AS t3 " <>
            "WHERE t3.cate_id=? ORDER BY publish_date DESC":: Text
  values <- getCateContRaw cateid sql
  return $ Right (map toCateCont values)

getCateContRaw :: MonadIO m =>
  Int64
  -> Text
  -> ReaderT E.SqlBackend m [
  ( E.Single Int64
  , E.Single Int
  , E.Single Int64
  , E.Single Text
  , E.Single UTCTime)]
getCateContRaw cateid sql = E.rawSql sql
    [
      E.PersistInt64 $ fromIntegral (fromEnum TypePost)
    , E.PersistInt64 $ fromIntegral (fromEnum Published)
    , E.PersistInt64 $ fromIntegral (fromEnum TypeFree)
    , E.PersistInt64 $ fromIntegral (fromEnum Published)
    , E.PersistInt64 cateid
    ]

recursiveCateEntity :: MonadIO m =>
  Entity TblCategory
  -> ReaderT SqlBackend m [ Entity TblCategory ]
recursiveCateEntity (Entity _ cate) = do
  case tblCategoryParentId cate of
    Just pid -> do
      res <- getEntity $ toTblCategoryKey (intToInt64 pid)
      case res of
        Just c -> do
          cs <- recursiveCateEntity c
          return $ cs ++ [c]
        Nothing -> return []
    Nothing -> return []
