{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.S3.Types
    ( S3URL (..)
    , S3Keys (..)
    , S3Method (..)
    , S3Request (..)
    , S3Header
    , getS3Header
    , s3Header
    , s3HeaderBuilder
    ) where

import           Data.ByteString.UTF8     (ByteString)
import           GHC.Generics             (Generic)
import           Data.Char8               (isSpace)
import           Blaze.ByteString.Builder (Builder, fromByteString)
import           Data.Monoid              (mconcat)
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive  as CI

newtype S3Header = S3Header { getS3Header :: (ByteString, ByteString) }
  deriving (Generic, Show)

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


trim :: ByteString -> ByteString
trim = BS.dropWhile isSpace . fst . BS.spanEnd isSpace

s3Header :: ByteString -> ByteString -> S3Header
s3Header header value = S3Header (lower, trimmed)
  where
    lower   = CI.foldCase header
    trimmed = trim value

s3HeaderBuilder :: S3Header -> Builder
s3HeaderBuilder (S3Header (header,value)) =
  mconcat [fromByteString header, ":", fromByteString value, "\n"]
