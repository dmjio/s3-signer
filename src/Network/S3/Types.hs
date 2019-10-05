{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.S3.Types
    ( S3Method (..)
    , S3Request (..)
    , S3SignedRequest (..)
    , S3Header
    , getS3Header
    , s3Header
    , s3HeaderBuilder
    , emptyHash
    ) where

import           Data.ByteString.UTF8     (ByteString)
import           GHC.Generics             (Generic)
import           Data.Char                (isSpace)
import           Network.HTTP.Types       (Query)
import           Data.Time.Clock          (UTCTime)

import           Blaze.ByteString.Builder (Builder, fromByteString)
import           Data.Monoid              (mconcat)
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI


newtype S3Header = S3Header { getS3Header :: (ByteString, ByteString) }
  deriving (Generic, Show)


data S3Method = S3GET    -- ^ GET Request
              | S3PUT    -- ^ PUT Request
              | S3HEAD   -- ^ HEAD Request
              | S3DELETE -- ^ DELETE Request
    deriving (Generic, Show)


data S3Request = S3Request {
      s3method    :: S3Method         -- ^ Type of HTTP Method
    , mimeType    :: Maybe ByteString -- ^ MIME Type
    , bucketName  :: ByteString       -- ^ Name of Amazon S3 Bucket
    , objectName  :: ByteString       -- ^ Name of Amazon S3 File
    , regionName  :: ByteString       -- ^ Name of Amazon S3 Region
    , queryString :: Query            -- ^ Optional query string items
    , requestTime :: UTCTime          -- ^ Requests are valid within a 15 minute window of this timestamp
    , payloadHash :: Maybe ByteString -- ^ SHA256 hash string of the payload; Nothing if unsigned
    , s3headers   :: [S3Header]       -- ^ Headers
    , expires     :: Int              -- ^ Expiration in seconds
    , s3Key         :: ByteString
    , s3Secret      :: ByteString
    } deriving (Generic, Show)

-- | Hash of empty payload
emptyHash :: ByteString
emptyHash = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

data S3SignedRequest = S3SignedRequest {
      sigHeaders    :: [S3Header] -- ^ The headers included in the signed request
    , sigDate       :: ByteString -- ^ The date you used in creating the signing key.
    , sigCredential :: ByteString -- ^ <your-access-key-id>/<date>/<aws-region>/<aws-service>/aws4_request
    , sigPolicy     :: ByteString -- ^ The Base64-encoded security policy that describes what is permitted in the request
    , sigSignature  :: ByteString -- ^ (AWS Signature Version 4) The HMAC-SHA256 hash of the security policy.
    , sigAlgorithm  :: ByteString -- ^ The signing algorithm used. For AWS Signature Version 4, the value is AWS4-HMAC-SHA256.
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

