{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.S3
    ( generateS3URL
    , S3Creds (..)
    , S3RequestType (S3GET, S3PUT)
    , S3URL (..)
    , S3Request (..)
    ) where

import qualified Data.Text.Lazy          as T
import qualified Data.Text.Lazy.Encoding as T

import           Network.S3.Sign
import           Network.S3.Time
import           Network.S3.Types
import           Network.S3.URL

-- | Signer
generateS3URL :: S3Creds -> S3Request -> IO S3URL
generateS3URL S3Creds{..} S3Request{..} = do
  time <- getExpirationTimeStamp secondsToExpire
  let url = case requestType of
              S3GET -> getURL bucketName objectName time
              S3PUT -> putURL bucketName objectName time
      req = T.decodeUtf8 $ s3URL bucketName objectName publicKey time (sign secretKey url)
  return $ S3URL (T.toStrict req)



