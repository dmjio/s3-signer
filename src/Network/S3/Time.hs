module Network.S3.Time
    ( getExpirationTimeStamp
    , utcTimeToEpochTime
    ) where

import           Control.Applicative   ((<$>))
import           Data.ByteString.UTF8  (ByteString, fromString)
import           Data.Time             (UTCTime (..), getCurrentTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

getExpirationTimeStamp :: Integer -> IO ByteString
getExpirationTimeStamp secs = fromString . show . (+secs) . utcTimeToEpochTime <$> getCurrentTime

utcTimeToEpochTime :: UTCTime -> Integer
utcTimeToEpochTime = fromIntegral . toSecs
  where
    toSecs :: UTCTime -> Int
    toSecs = round . utcTimeToPOSIXSeconds
