module Network.S3.Time
    ( getExpirationTimeStamp
    , utcTimeToEpochTime
    ) where

import           Control.Applicative   ((<$>))
import           Control.Monad.Time    (MonadTime (..))
import           Data.ByteString.UTF8  (ByteString, fromString)
import           Data.Time             (UTCTime (..))
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

getExpirationTimeStamp :: MonadTime m => Integer -> m ByteString
getExpirationTimeStamp secs = fromString . show . (+secs) . utcTimeToEpochTime <$> currentTime

utcTimeToEpochTime :: UTCTime -> Integer
utcTimeToEpochTime = fromIntegral . toSecs
  where
    toSecs :: UTCTime -> Int
    toSecs = round . utcTimeToPOSIXSeconds
