{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.S3.URL
    ( canonicalRequest
    ) where

import           Data.Time.Format         (formatTime, defaultTimeLocale)
import           Data.ByteString.Char8    (pack)
import           Blaze.ByteString.Builder (Builder, fromByteString)

import qualified Network.HTTP.Types.URI as HTTP
import           Data.Function (on)
import           Data.List (sortBy, intersperse)
import           Data.Monoid ((<>))
import           Data.Maybe (fromMaybe)

import           Network.S3.Types

sortS3Headers :: [S3Header] -> [S3Header]
sortS3Headers = sortBy (compare `on` (fst . getS3Header))


-- | Build a canonical request to be signed
--   AWS docs: https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html#canonical-request
canonicalRequest :: S3Request -> Builder
canonicalRequest S3Request{..} =
  let
    -- We MUST sort the parameters in the query string alphabetically by key name
    qs         = sortBy (compare `on` fst) queryString
    emptyHash  = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    bodyHash   = fromMaybe emptyHash payloadHash
    hashHeader = s3Header "x-amz-content-sha256" bodyHash
    hostHeader = s3Header "host" (bucketName <> ".s3.amazonaws.com")
    seconds    = pack (formatTime defaultTimeLocale "T%M%H%SZ" requestTime)
    date       = pack (formatTime defaultTimeLocale "%Y%m%d" requestTime)
    timeHeader = s3Header "x-amz-date" (date <> seconds)
    headers    = sortS3Headers (timeHeader : hostHeader : hashHeader : s3headers)
    headerKeys = map (fst . getS3Header) headers

    httpMethod       = renderS3Method s3method
    canonicalURI     = HTTP.urlEncodeBuilder False objectName
    canonicalQS      = HTTP.renderQueryText False (HTTP.queryToQueryText qs)
    canonicalHeaders = foldMap s3HeaderBuilder headers
    signedHeaders    = foldMap fromByteString (intersperse ";" headerKeys)
    hashedPayload    = fromByteString bodyHash

    uriBuilder =
      httpMethod       <> "\n/" <>
      canonicalURI     <> "\n" <>
      canonicalQS      <> "\n" <>
      canonicalHeaders <> "\n" <>
      signedHeaders    <> "\n" <>
      hashedPayload
  in
    uriBuilder


renderS3Method :: S3Method -> Builder
renderS3Method method =
  case method of
    S3GET    -> "GET"
    S3PUT    -> "PUT"
    S3HEAD   -> "HEAD"
    S3DELETE -> "DELETE"
