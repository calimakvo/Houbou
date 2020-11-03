{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import           ClassyPrelude.Yesod
import qualified Control.Exception           as Exception
import           Data.Aeson                  (Result (..), fromJSON, withObject,
                                              (.!=), (.:?))
import           Data.FileEmbed              (embedFile)
import           Data.Yaml                   (decodeEither')
import           Database.Persist.Postgresql (PostgresConf)
import           Language.Haskell.TH.Syntax  (Exp, Name, Q)
import           Network.Wai.Handler.Warp    (HostPreference)
import           Yesod.Default.Config2       (applyEnvValue, configSettingsYml)
import           Yesod.Default.Util          (WidgetFileSettings,
                                              widgetFileNoReload,
                                              widgetFileReload)
import           Database.Redis.Config (RedisConfig (..))

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appStaticDir              :: String
    -- ^ Directory from which to serve static files.
    , appDatabaseConf           :: PostgresConf
    -- ^ Configuration settings for accessing the database.
    , appRedisConf              :: RedisConfig
    -- ^ Configuration settings for accessing the redis.
    , appRoot                   :: Maybe Text
    -- ^ Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    , appReloadTemplates        :: Bool
    -- ^ Use the reload version of templates
    , appMutableStatic          :: Bool
    -- ^ Assume that files in the static dir may change after compilation
    , appSkipCombining          :: Bool
    -- ^ Perform no stylesheet/script combining

    -- Example app-specific configuration values.
    , appCopyright              :: Text
    -- ^ Copyright text to appear in the footer of the page
    , appAnalytics              :: Maybe Text
    -- ^ Google Analytics code

    , appBlogSettingId          :: Text
    -- ^ Blog setting record UNIQE key
    , appSessionTimeOut         :: Int
    -- ^ Session Time out
    , appLoginEmailLength       :: Int
    -- ^ loginid email length
    , appLoginPasswordLength    :: Int
    -- ^ password length
    , appPagePerLineSelector    :: [Int]
    -- ^ Post title length
    , appPostTitleMaxLength :: Int
    -- ^ Post Text length
    , appPostTextMaxLength :: Int
    -- ^ Slug Text length
    , appSlugTextMaxLength :: Int
    -- ^ Post tag length
    , appTagMaxLength :: Int
    -- ^ Post tag one length
    , appTagOneMaxLength :: Int
    -- ^ Frame name length
    , appFrameNameMaxLength :: Int
    -- ^ Frame html length
    , appFrameHtmlMaxLength :: Int
    -- ^ Frame css length
    , appFrameCssMaxLength :: Int
    -- ^ Free page title length
    , appFreeTitleMaxLength :: Int
    -- ^ Free page contents length
    , appFreeContentMaxLength :: Int
    -- ^ Free page css length
    , appFreeCssMaxLength :: Int
    -- ^ Blog set blogname length
    , appBlogSetBlogNameMaxLength :: Int
    -- ^ Blog set blogurl length
    , appBlogSetBlogUrlMaxLength :: Int
    -- ^ Blog set postnum max
    , appBlogSetPostNumMax :: Int
    -- ^ Blog set media url length
    , appBlogSetMediaUrlMaxLength :: Int
    -- ^ Blog set media directory length
    , appBlogSetMediaDirMaxLength :: Int
    -- ^ Blog set upload request size max
    , appBlogSetUploadSizeMax :: Int
    -- ^ Blog set adstxt length
    , appBlogSetAdsTxtMaxLength :: Int
    -- ^ Blog set user account email length
    , appUserAccEmailMaxLength :: Int
    -- ^ Blog set user account pass min length
    , appUserAccPasswdMinLength :: Int
    -- ^ Blog set user account pass max length
    , appUserAccPasswdMaxLength :: Int
    -- ^ Blog set user account name length
    , appUserAccUserNameMaxLength :: Int
    -- ^ Blog set user account profile length
    , appUserAccUserProfMaxLength :: Int
    -- ^ Set media title length
    , appMediaTitleMaxLength :: Int
    -- ^ Set meta description length
    , appMetaDescriptionMaxLength :: Int
    -- ^ Set meta keywords length
    , appMetaKeywordsMaxLength :: Int
    -- ^ Set meta Robots length
    , appMetaRobotsMaxLength :: Int
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#ifdef DEVELOPMENT
                True
#else
                False
#endif
        appStaticDir              <- o .: "static-dir"
        appDatabaseConf           <- o .: "database"
        fromYamlAppRedisConf      <- o .: "redis"
        appRoot                   <- o .:? "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        dev                       <- o .:? "development"      .!= defaultDev

        appDetailedRequestLogging <- o .:? "detailed-logging" .!= dev
        appShouldLogAll           <- o .:? "should-log-all"   .!= dev
        appReloadTemplates        <- o .:? "reload-templates" .!= dev
        appMutableStatic          <- o .:? "mutable-static"   .!= dev
        appSkipCombining          <- o .:? "skip-combining"   .!= dev

        appCopyright              <- o .:  "copyright"
        appAnalytics              <- o .:? "analytics"

        appBlogSettingId          <- o .: "blog-setting-id"
        appSessionTimeOut         <- o .: "session-timeout"
        appLoginEmailLength       <- o .: "login-email-length"
        appLoginPasswordLength    <- o .: "login-passwd-length"
        appPagePerLineSelector    <- o .: "list-selector"
        appPostTitleMaxLength     <- o .: "post-title-length"
        appPostTextMaxLength      <- o .: "post-text-length"
        appSlugTextMaxLength      <- o .: "slug-text-length"
        appTagMaxLength           <- o .: "tag-length"
        appTagOneMaxLength        <- o .: "tag-one-length"
        appFrameNameMaxLength     <- o .: "frame-name-length"
        appFrameHtmlMaxLength     <- o .: "frame-html-length"
        appFrameCssMaxLength      <- o .: "frame-css-length"
        appFreeTitleMaxLength     <- o .: "free-title-length"
        appFreeContentMaxLength   <- o .: "free-html-length"
        appFreeCssMaxLength       <- o .: "free-css-length"
        appBlogSetBlogNameMaxLength <- o .: "blog-set-blog-name-length"
        appBlogSetBlogUrlMaxLength <- o .: "blog-set-blog-url-length"
        appBlogSetPostNumMax      <- o .: "blog-set-post-num-max"
        appBlogSetMediaUrlMaxLength <- o .: "blog-set-media-url-length"
        appBlogSetMediaDirMaxLength <- o .: "blog-set-media-dir-length"
        appBlogSetUploadSizeMax     <- o .: "blog-set-upload-size-max"
        appBlogSetAdsTxtMaxLength   <- o .: "blog-set-adstxt-length"
        appUserAccEmailMaxLength    <- o .: "user-acc-email-length"
        appUserAccPasswdMinLength   <- o .: "user-acc-passwd-min-length"
        appUserAccPasswdMaxLength   <- o .: "user-acc-passwd-max-length"
        appUserAccUserNameMaxLength <- o .: "user-acc-username-length"
        appUserAccUserProfMaxLength <- o .: "user-acc-profile-length"
        appMediaTitleMaxLength      <- o .: "media-title-length"
        appMetaDescriptionMaxLength <- o .: "meta-length-description"
        appMetaKeywordsMaxLength    <- o .: "meta-length-keywords"
        appMetaRobotsMaxLength      <- o .: "meta-length-robots"

        let appRedisConf =
              RedisConfig {
                getConnectInfo = getConnectInfo fromYamlAppRedisConf
                }

        return AppSettings {..}

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if appReloadTemplates compileTimeAppSettings
                then widgetFileReload
                else widgetFileNoReload)
              widgetFileSettings

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = either Exception.throw id
                       $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e -> error e
        Success settings -> settings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings
