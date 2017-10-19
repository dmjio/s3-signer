{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.S3
    ( -- * Create pre-signed AWS S3 URL
      generateS3URL
      -- * Types
    , module Network.S3.Types
    ) where

import           Data.ByteString (ByteString)
import           Data.Time.Clock (getCurrentTime)
import           Network.S3.Sign
import           Network.S3.Time
import           Network.S3.Types
import           Network.S3.URL


generateS3URL :: ByteString -- ^ Amazon S3 SecretAccessKey
              -> S3Request -- ^ Amazon S3 Request information
              -> IO S3SignedRequest -- ^ Generated Request
generateS3URL secretKey req = do
  time <- getCurrentTime
  return (sign secretKey req time)
