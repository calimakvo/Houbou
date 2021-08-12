{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

module Foundation where

import Import.NoFoundation
import qualified Prelude as P
import Database.Persist.Sql (ConnectionPool, Single, runSqlPool, rawSql, unSingle)
import Data.Text as T
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Control.Monad.Logger (LogSource)
import Text.Email.Validate (isValid)

import qualified Yesod.Auth.Message as M
import Yesod.Auth.HashDB (authHashDBWithForm, HashDBUser(..))
import Database.Persist.Sql (BackendKey(..))
import qualified Database.Redis as R
import Database.Redis.Config (RedisConfig (..))
import Web.ServerSession.Backend.Redis (RedisStorage(..))

import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Web.ServerSession.Frontend.Yesod as S
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE

import DataTypes.HoubouType
import Libs.Common
import UrlParam.Page
import UrlParam.PostId
import UrlParam.FrameId
import UrlParam.FreeId
import UrlParam.FreeFrameId
import UrlParam.UserId
import UrlParam.MediaId
import UrlParam.TagId
import UrlParam.YPath
import UrlParam.MPath
import UrlParam.DPath
import UrlParam.Slug
import UrlParam.CateId

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadUnliftIO m) => ReaderT SqlBackend m a

-- | Login form data
data LoginForm = LoginForm {
    unLoginId :: Text
  , unPassword :: Text
  , unRemeberCheck :: Maybe Bool
} deriving(Show, Eq)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend = makeSessionBackendRedis

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware app = defaultYesodMiddleware $ defaultCsrfMiddleware $ app

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        muser <- maybeAuthPair
        (pall, ppub, pdra) <- postCountAll
        (fall, fpub, fdra) <- freeCountAll
        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_min_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute
        :: App
        -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized RssR _ = return Authorized
    isAuthorized (PutR _) _ = return Authorized
    isAuthorized (PutSlugR _ _ _ _) _ = return Authorized
    isAuthorized (PutFreeR _) _ = return Authorized
    isAuthorized (PutFreeSlugR _ _ _ _) _ = return Authorized
    isAuthorized (PutTagListR _) _ = return Authorized
    isAuthorized (PutCateListR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized AdstxtR _ = return Authorized
    isAuthorized HbAdminRootR _ = return Authorized
    isAuthorized SitemapR _ = return Authorized

    -- Admin
    isAuthorized DashboardR _ = isAuthenticated
    isAuthorized PostListR _ = isAuthenticated
    isAuthorized (PostR _) _ = isAuthenticated
    isAuthorized PostNewR _ = isAuthenticated
    isAuthorized PostDelR _ = isAuthenticated
    isAuthorized StatusChangeR _ = isAuthenticated
    isAuthorized FreeStatusChangeR _ = isAuthenticated
    isAuthorized FrameChangeR _ = isAuthenticated
    isAuthorized FreeFrameChangeR _ = isAuthenticated
    isAuthorized (FrameR _) _ = isAuthenticated
    isAuthorized (FrameListR _) _ = isAuthenticated
    isAuthorized FrameNewR _ = isAuthenticated
    isAuthorized FrameDelR _ = isAuthenticated
    isAuthorized (FreeFrameListR _) _ = isAuthenticated
    isAuthorized (FreeFrameR _) _ = isAuthenticated
    isAuthorized FreeFrameNewR _ = isAuthenticated
    isAuthorized FreeFrameDelR _ = isAuthenticated
    isAuthorized (FreeR _) _ = isAuthenticated
    isAuthorized FreeDelR _ = isAuthenticated
    isAuthorized FreeListR _ = isAuthenticated
    isAuthorized FreeNewR _ = isAuthenticated
    isAuthorized BlogSettingR _ = isAuthenticated
    isAuthorized (UserListR _) _ = isAuthenticated
    isAuthorized (UserR _) _ = isAuthenticated
    isAuthorized UserNewR _ = isAuthenticated
    isAuthorized UserDelR _= isAuthenticated
    isAuthorized UserDelResetR _ = isAuthenticated
    isAuthorized (MediaListR _) _ = isAuthenticated
    isAuthorized (MediaInsR _) _ = isAuthenticated
    isAuthorized MediaNewR _ = isAuthenticated
    isAuthorized MediaNewPopR _ = isAuthenticated
    isAuthorized MediaDelR _ = isAuthenticated
    isAuthorized (MediaMdfR _) _ = isAuthenticated
    isAuthorized PostPrevR _ = isAuthenticated
    isAuthorized FreePrevR _ = isAuthenticated
    isAuthorized CategoryR _ = isAuthenticated
    isAuthorized CateListR _ = isAuthenticated
    isAuthorized (CateNewR _) _ = isAuthenticated
    isAuthorized (CateModR _) _ = isAuthenticated
    isAuthorized (CateDelR _) _ = isAuthenticated
    isAuthorized PostListSearchR _ = isAuthenticated
    isAuthorized FreeListSearchR _ = isAuthenticated
    isAuthorized AccDayR _ = isAuthenticated

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

    maximumContentLengthIO :: App -> Maybe (Route App) -> IO (Maybe Word64)
    maximumContentLengthIO master _ = do
        let setId = appBlogSettingId $ appSettings master
        result <- runSqlPool (getBy $ UniIdent setId) (appConnPool master)
        case result of
          Just (Entity _ setting) -> return $ Just $
            fromIntegral ((tblBlogSettingUploadSize setting) * 1024 * 1024)
          Nothing -> error "Uninitialized blog setting."

    errorHandler :: ErrorResponse -> Handler TypedContent
    errorHandler NotFound = customNotFound NotFound
    errorHandler other = defaultErrorHandler other

customNotFound :: ErrorResponse -> Handler TypedContent
customNotFound _ = fmap toTypedContent $ genericLayout $ do
  setTitle "Not Found"
  $(widgetFile "notfound")
  

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = TblUserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = DashboardR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = AuthR LoginR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = False

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
        x <- getBy $ UniTblUserEmail $ credsIdent creds
        case x of
            Just (Entity uid entity) -> do
              if tblUserDeleteFlag entity == False
              then return $ Authenticated uid
              else return $ UserError $ M.InvalidEmailAddress
            Nothing -> return $ UserError $ M.IdentifierNotFound "User not found"

    onLogout :: (MonadHandler m, master ~ HandlerSite m) => m ()
    onLogout = return ()

    authLayout :: (MonadHandler m, HandlerSite m ~ App) => WidgetFor App () -> m Html
    authLayout = liftHandler . loginLayout

    authPlugins :: App -> [AuthPlugin App]
    authPlugins _ = [authHashDBWithForm loginForm (Just . UniTblUserEmail)]

    renderAuthMessage :: master -> [Text] -> M.AuthMessage -> Text
    renderAuthMessage _ _ = M.japaneseMessage

instance HashDBUser TblUser where
    userPasswordHash = tblUserPassword
    setPasswordHash h u = u { tblUserPassword = Just h }

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

-- | Session Settings
makeSessionBackendRedis :: App -> IO (Maybe SessionBackend)
makeSessionBackendRedis master = do
    let redisConf = appRedisConf $ appSettings master
    timeOut <- getHbSessionTimeout master
    S.simpleBackend (opts timeOut) =<< redisStorage redisConf
        where opts to =
                S.setIdleTimeout (Just $ (fromIntegral to) * 60)  -- minutes
                . S.setAbsoluteTimeout (Just $  2 * 60 * 60 * 24) -- 2 days minutes
                . S.setCookieName sessionCookieName
                . S.setHttpOnlyCookies True
                . S.setSecureCookies True

getHbSessionTimeout :: App -> IO Int
getHbSessionTimeout master = do
  let setId = appBlogSettingId $ appSettings master
  result <- runSqlPool (getBy $ UniIdent setId) (appConnPool master)
  case result of
    Just (Entity _ setting) -> return $ tblBlogSettingSessionTimeout setting
    Nothing -> error "Uninitialized blog setting."

-- | Session strage for redis connection
redisStorage :: RedisConfig -> IO (RedisStorage sess)
redisStorage rc = do
    conn <- R.connect $ getConnectInfo rc
    return $ RedisStorage conn Nothing Nothing

-- | Cookie name used for the sessions of this example app.
sessionCookieName :: Text
sessionCookieName = "HB_SESSION"

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ MsgSelectNone = "選択してください"
    renderMessage _ _ MsgValueRequired = "入力は必須です"
    renderMessage _ _ fm = defaultFormMessage fm

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

houbouMiddleware :: Handler res -> Handler res
houbouMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware

genericLayout :: Widget -> Handler Html
genericLayout widget = do
  master <- getYesod
  pc <- widgetToPageContent $ do
    addStylesheet $ StaticR css_bootstrap_min_css
    $(widgetFile "generic-default-layout")
  withUrlRenderer $(hamletFile "templates/generic-layout-wrapper.hamlet")

loginLayout :: Widget -> Handler Html
loginLayout widget = do
    master <- getYesod
    pc <- widgetToPageContent $ do
        addStylesheet $ StaticR css_bootstrap_min_css
        $(widgetFile "generic-default-layout")
    withUrlRenderer $(hamletFile "templates/generic-layout-wrapper.hamlet")

loginForm :: Yesod App => Route App -> WidgetFor App ()
loginForm loginRoute = do
    msg <- getMessages
    (widget, _) <- liftHandler $ generateFormPost $ lgnForm
    setTitle "Houbou - ログイン"
    toWidgetHead
        [hamlet|
            <meta name="keywords" content="ログイン">
            <meta name="description" content="ログイン">
        |]
    $(whamletFile "templates/lgnform.hamlet")

lgnForm :: Html -> MForm Handler (FormResult LoginForm, Widget)
lgnForm extra = do
    let loginIdFieldSet = FieldSettings {
            fsLabel = "",
            fsTooltip = Nothing,
            fsId = Just "username",
            fsName = Just "username",
            fsAttrs = [
                ("class", "form-control"),
                ("required", ""),
                ("autocomplete", "email"),
                ("placeholder", "E-mail"),
                ("autofocus", "")
            ]
        }
        passwdFieldSet = FieldSettings {
            fsLabel = "",
            fsTooltip = Nothing,
            fsId = Nothing,
            fsName = Just "password",
            fsAttrs = [
                ("class", "form-control"),
                ("required", ""),
                ("autocomplete", "current-password"),
                ("placeholder", "password")
            ]
        }
        rememberFieldSet = FieldSettings {
            fsLabel = "Remember Me",
            fsTooltip = Nothing,
            fsId = Nothing,
            fsName = Just "remember",
            fsAttrs = [
                ("class", "")
            ]
        }
    master <- getYesod
    let emailLen = appLoginEmailLength $ appSettings master
        passwdLen = appLoginPasswordLength $ appSettings master
    (emailRes, emailView) <- mreq (emailLoginIdField emailLen) loginIdFieldSet Nothing
    (passwdRes, passwdView) <- mreq (passPasswordField passwdLen) passwdFieldSet Nothing
    (rememRes, rememView) <- mopt checkBoxField rememberFieldSet Nothing
    let loginFormParam = LoginForm <$> emailRes <*> passwdRes <*> rememRes
    let widget = do
            [whamlet|
                #{extra}
                <div .form-group>
                    ^{fvInput emailView}
                    $case emailRes
                        $of FormFailure err
                            <p>#{T.unlines err}
                        $of _

                <div .form-group>
                    ^{fvInput passwdView}

                <div .checkbox>
                    <label>
                        ^{fvInput rememView}
                            ^{fvLabel rememView}

                <button .btn .btn-lg .btn-success .btn-block>Login
            |]
    return (loginFormParam, widget)

errorMessage :: Text
errorMessage = "ID・パスワードが不正です"

emailLoginIdField :: Int -> Field Handler Text
emailLoginIdField emailLen = check (validateEmail emailLen) textField

validateEmail :: Int -> Text -> Either Text Text
validateEmail emailLen email
    | T.length(email) > emailLen && (isValid $ TE.encodeUtf8(email)) == False
          = Left errorMessage
    | otherwise = Right email

passPasswordField :: Int -> Field Handler Text
passPasswordField passwdLen = check (validatePasswd passwdLen) passwordField

validatePasswd :: Int -> Text -> Either Text Text
validatePasswd passwdLen passwd
    | T.length(passwd) > passwdLen = Left errorMessage
    | otherwise = Right passwd

publishIntState :: (Int64, Int64, Int64)
publishIntState =
  (
    fromIntegral $ fromEnum Published
  , fromIntegral $ fromEnum Draft
  , fromIntegral $ fromEnum UnPublished
  )

postCountAll :: Handler (Int, Int, Int)
postCountAll = do
  let s1 = "SELECT COUNT(id) FROM tbl_post WHERE (status = ? OR status = ?)" :: Text
      s2 = "SELECT COUNT(id) FROM tbl_post WHERE status = ?" :: Text
      (published, draft, unpublished) = publishIntState
  runDB $ do
    pall <- unSgl <$> blogCount s1 [PersistInt64 published, PersistInt64 unpublished]
    pc <- unSgl <$> blogCount s2 [PersistInt64 published]
    pd <- unSgl <$> blogCount s2 [PersistInt64 draft]
    return (pall, pc, pd)

freeCountAll :: Handler (Int, Int, Int)
freeCountAll = do
  let s1 = "SELECT COUNT(id) FROM tbl_free WHERE (status = ? OR status = ?)" :: Text
      s2 = "SELECT COUNT(id) FROM tbl_free WHERE status = ?" :: Text
      (published, draft, unpublished) = publishIntState
  runDB $ do
    fall <- unSgl <$> blogCount s1 [PersistInt64 published, PersistInt64 unpublished]
    fc <- unSgl <$> blogCount s2 [PersistInt64 published]
    fd <- unSgl <$> blogCount s2 [PersistInt64 draft]
    return (fall, fc, fd)

blogCount :: MonadIO m =>
  Text
  -> [PersistValue]
  -> ReaderT SqlBackend m [ Single Int ]
blogCount sql ps = rawSql sql ps

unSgl :: [ Single Int ] -> Int
unSgl = unSingle . P.head
