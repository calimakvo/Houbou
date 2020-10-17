{-# LANGUAGE OverloadedStrings #-}

module Libs.Common (
    getTm
  , textToInt
  , intToInt64
  , rowClass
  , timeZoneHours
  , dateTimeFullFormat
  , dateFormat
  , dateFormatYM
  , dateFormatNonSlash
  , dateFormatNonSlashTime
  , dateZonedTimeFullFormat
  , timeZone
  , localTime
  , dummyUtc
  , paginator
  , searchType
  , successKey
  , errorKey
  , listTypeKey
  , pageparline
  , userparline
  , framepageperline
  , freepageperline
  , maybeToText
  , toMaybeText
  , toText
  , encryptPasswd
  , defaultStrength
  , rootUserId
  , rmSlash
  , maybeReadInt
  , uploadSizeMin
  , abridgeText
  , showSize
  , showImgUrl
  , mediaPath
  , pagePath
  , utcToJpTime
  , toGrecoFull
  , toGrecoFullLocal
  , secZeroDatetime
  , splitTags
  , dayAccessToUnixTime
  , createBar1Data
  , createBar2Data
  , toUrlPath
  , parmErrToMsg
  , rmLfCr
) where

import Data.Int
import Data.Text
import Data.Time
import Data.Maybe
import Text.Regex
import Text.Printf
import Foreign.C.Types (CTime(..))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import System.Posix.Types (EpochTime)
import qualified Data.DateTime as D
import Data.Fixed
import qualified Data.ByteString.Char8 as B
import Yesod.Auth.Util.PasswordStore (makePassword)
import DataTypes.HoubouType

rootUserId ::
  Int64
rootUserId = 1

uploadSizeMin ::
  Int
uploadSizeMin = 2

rowClass ::
  Int
  -> Text
rowClass n
  | even n == True = "even"
  | otherwise = "odd"

textToInt ::
  Text
  -> Int
textToInt = read . unpack

intToInt64 ::
  Int
  -> Int64
intToInt64 = fromIntegral

getTm :: IO UTCTime
getTm = getZonedTime >>= return . zonedTimeToUTC

dateTimeFullFormat ::
  UTCTime
  -> Text
dateTimeFullFormat t = pack (formatTime defaultTimeLocale "%Y/%m/%d %T" (zoneTime t))

dateFormat ::
  UTCTime
  -> Text
dateFormat t = pack (formatTime defaultTimeLocale "%Y/%m/%d" (zoneTime t))

dateFormatYM ::
  UTCTime
  -> Text
dateFormatYM t = pack (formatTime defaultTimeLocale "%Y/%m" (zoneTime t))

dateFormatNonSlash ::
  UTCTime
  -> Text
dateFormatNonSlash t = pack (formatTime defaultTimeLocale "%Y%m%d" (zoneTime t))

dateFormatNonSlashTime ::
  UTCTime
  -> Text
dateFormatNonSlashTime t = pack (formatTime defaultTimeLocale "%Y%m%d_%H%M%S" (zoneTime t))

dateZonedTimeFullFormat ::
  UTCTime
  -> Text
dateZonedTimeFullFormat t = pack (formatTime defaultTimeLocale "%Y/%m/%d %T" (zoneTime t))

zoneTime ::
  UTCTime
  -> ZonedTime
zoneTime utcTime = utcToZonedTime timeZone utcTime

timeZoneHours :: Int
timeZoneHours = 9

timeZone ::
  TimeZone
timeZone = TimeZone (60 * timeZoneHours) False "JST"

localTime ::
  UTCTime
  -> LocalTime
localTime utcTime = utcToLocalTime timeZone utcTime

utcToJpTime ::
  UTCTime
  -> LocalTime
utcToJpTime ut = utcToLocalTime timeZone ut

toGrecoFullLocal ::
  LocalTime
  -> (Integer, Int, Int, Int, Int, Int)
toGrecoFullLocal lt =
  let (y, m, d) = toGregorian (localDay lt)
      t = localTimeOfDay lt
      (h, m', s) = (todHour t, todMin t, todSec t)
  in (y, m, d, h, m', toPicoInt s)

toPicoInt ::
  Pico
  -> Int 
toPicoInt pico = truncate val
  where val = fromRational (toRational pico) :: Double

toGrecoFull ::
  D.DateTime
  -> (Integer, Int, Int, Int, Int, Int)
toGrecoFull ut = D.toGregorian $ zonedTimeToUTC (zoneTime ut)

secZeroDatetime ::
  (Integer, Int, Int, Int, Int, Int)
  -> D.DateTime
secZeroDatetime (y, m, d, h, m', _) =
  let base = (m' `div` 5) * 5
  in D.fromGregorian y m d h base 0

paginator ::
  Int
  -> Int
  -> Int
  -> Paginator
paginator curPage total ppLine =
    Paginator {
        unIsPager = isPager,
        unCurPage = curPage,
        unPrevPage = prevPage,
        unNumsPages = [start..end],
        unNextPage = nextPage
    }
    where
        isPager = total > ppLine
        start = if curPage - pageMinus < pageStart then pageStart else curPage - pageMinus
        end = if start + pagePlus > maxPage then maxPage else start + pagePlus
        prevPage = if start >= (pageMinus - 1) then start - 1 else 0
        nextPage = if start + pagePlus > maxPage then 0 else start + pagePlus + 1
        maxPage = ceiling ((fromIntegral total / fromIntegral ppLine) :: Double)
        pageMinus = 3
        pagePlus = 6
        pageStart = 1

searchType ::
  Int
  -> (PubViewStatus, Text)
searchType 1 = (ViewAll, "すべて")
searchType 2 = (ViewPublished, "公開済")
searchType _ = (ViewAll, "すべて")

successKey ::
  Text
successKey = "success"

errorKey ::
  Text
errorKey = "error"

listTypeKey ::
  Text
listTypeKey = "truePageType"

pageparline ::
  Text
pageparline = "ppline"

userparline ::
  Text
userparline = "userpline"

framepageperline ::
  Text
framepageperline = "frameppline"

freepageperline ::
  Text
freepageperline = "freeppline"

maybeToText ::
  Maybe Text
  -> Text
maybeToText = fromMaybe empty

toMaybeText ::
  Text
  -> Maybe Text
toMaybeText html
  | (not $ Data.Text.null html == True) = Just html
  | otherwise = Nothing

toText  :: Show a =>
  a
  -> Text
toText = pack . show

encryptPasswd ::
  Int
  -> Text
  -> IO Text
encryptPasswd strength pwd = do
    h <- makePassword (B.pack $ unpack pwd) strength
    return $ pack $ B.unpack h

defaultStrength ::
  Int
defaultStrength = 17

dummyUtc ::
  UTCTime
dummyUtc = read "2020-08-04 06:44:18.000000 UTC"

rmSlash ::
  Text
  -> Text
rmSlash str = pack $ subRegex (mkRegex "/+") (unpack str) "/"

toUrlPath ::
  UTCTime
  -> Text
toUrlPath t = rmSlash $ dateFormat t <> "/"

mediaPath ::
  Text
  -> Text
  -> Text
  -> Text
mediaPath mediaUrl dir name =
  case takeEnd 1 mediaUrl of
    "/" -> mediaUrl <> (rmSlash $ dir <> "/" <> name)
    _ -> mediaUrl <> (rmSlash $ "/" <> dir <> "/" <> name)

-- #{pagePath (unBlogSettingBlogUrl setting) "p" (unPostId post)}
pagePath ::
  Text
  -> Text
  -> Int64
  -> Text
pagePath blogUrl typ pageId =
  case takeEnd 1 blogUrl of
    "/" -> blogUrl <> (rmSlash $ typ <> "/" <> toText(pageId))
    _ -> blogUrl <> (rmSlash $ "/" <> typ <> "/" <> toText(pageId))

abridgeText ::
  Int
  -> Text
  -> Text
abridgeText n str
  | Data.Text.length(str) > n = (Data.Text.take n str) <> "..."
  | otherwise = str

showSize ::
  Int
  -> Text
showSize byte
  | byte <= 1024                      = pack ((printf "%d" byte) ++ "B")
  | byte > 1024 && byte < 1024 * 1024 = pack ((printf "%.2f" (calc byte 1024) ++ "KB"))
  | byte >= 1024 * 1024               = pack ((printf "%.2f" (calc byte (1024 * 1024)) ++ "MB"))
  | otherwise = pack ((printf "%d" byte) ++ "B")

calc ::
  Int
  -> Int
  -> Double
calc byte size = (fromIntegral byte) / (fromIntegral size)

showImgUrl ::
  Text
  -> Text
  -> Text
  -> Text
showImgUrl setImgUrl dir name = abridgeText 200 (setImgUrl <> dir <> name)

maybeReadInt ::
  Text
  -> Maybe Int
maybeReadInt s = do
  case (listToMaybe . reads) (unpack s) of
    Just (num, "") -> Just num
    _ -> Nothing

splitTags ::
  TagStr
  -> TagList
splitTags = splitOn (pack ",")

dayAccessToUnixTime ::
  BlogAccess
  -> (EpochTime, Int)
dayAccessToUnixTime ba =
  (
    (1000*) <$> uToE $ UTCTime (unBlogAccessDate ba) 0 -- to ms
  , unBlogAccessTatolCnt ba
  )

uToE :: UTCTime -> EpochTime
uToE = CTime . truncate . utcTimeToPOSIXSeconds

createBar1Data ::
  [BlogAccess]
  -> [(EpochTime, Int)]
createBar1Data = Prelude.map dayAccessToUnixTime

createBar2Data ::
  [BlogAccess]
  -> [(EpochTime, Int)]
createBar2Data ba =
  Prelude.map (\(CTime t, c) -> (CTime $ t + chartOffset, c))
    (Prelude.map dayAccessToUnixTime ba)

chartOffset ::
  Int64
chartOffset = 43200000

parmErrToMsg ::
  ErrHoubou
  -> Text
parmErrToMsg err =
  case err of
    ErrRecVersion -> "データが更新されています、一覧を再読み込みして実行してください"
    ErrRecNotUnique -> "パーマリンクURLが重複しています"
    _ -> "エラーが発生しました"

rmLfCr :: Text -> Text
rmLfCr txt =  replace "\r" "" (replace "\n" "" txt)

