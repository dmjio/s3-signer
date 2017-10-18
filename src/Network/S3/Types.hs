{-# LANGUAGE DeriveGeneric #-}

module Network.S3.Types
    ( S3URL (..)
    , S3Keys (..)
    , S3Method (..)
    , S3Request (..)
    ) where

import           Data.ByteString.UTF8 (ByteString)
import           GHC.Generics         (Generic)

newtype S3URL = S3URL {
      signedRequest :: ByteString -- ^ Generated URL
    } deriving (Generic, Show)

data S3Keys = S3Keys {
      publicKey :: ByteString -- ^ AWS Public Key
    , secretKey :: ByteString  -- ^ AWS Private Key
    } deriving (Generic, Show)

data S3Method = S3GET    -- ^ GET Request
              | S3PUT    -- ^ PUT Request
              | S3HEAD   -- ^ HEAD Request
              | S3DELETE -- ^ DELETE Request
    deriving (Generic, Show)

data S3Request = S3Request {
      s3method        :: S3Method -- ^ Type of HTTP Method
    , mimeType        :: ByteString -- ^ MIME Type
    , bucketName      :: ByteString -- ^ Name of Amazon S3 Bucket
    , objectName      :: ByteString -- ^ Name of Amazon S3 File
    , secondsToExpire :: Integer -- ^ Number of seconds until expiration
    } deriving (Generic, Show)
