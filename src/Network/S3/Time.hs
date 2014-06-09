module Network.S3.Time
    ( getExpirationTimeStamp
    , utcTimeToEpochTime
    ) where

import           Control.Applicative        ((<$>))
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Time                  (UTCTime (..), getCurrentTime)
import           Data.Time.Clock.POSIX      (utcTimeToPOSIXSeconds)

getExpirationTimeStamp :: Integer -> IO ByteString
getExpirationTimeStamp secs = BL8.pack . show . (+secs) . utcTimeToEpochTime <$> getCurrentTime

utcTimeToEpochTime :: UTCTime -> Integer
utcTimeToEpochTime = fromIntegral . toSecs
  where
    toSecs :: UTCTime -> Int
    toSecs = round . utcTimeToPOSIXSeconds
