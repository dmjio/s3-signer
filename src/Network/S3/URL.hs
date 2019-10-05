{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.S3.URL
    ( canonicalRequest ) where

import           Blaze.ByteString.Builder (Builder, fromByteString)

import qualified Network.HTTP.Types.URI as HTTP
import           Data.Function (on)
import           Data.List (sortBy, intersperse)
import           Data.Monoid ((<>))
import           Data.Maybe (fromMaybe)

import           Network.S3.Types

sortS3Headers :: [S3Header] -> [S3Header]
sortS3Headers = sortBy (compare `on` (fst . getS3Header))

canonicalRequest :: S3Request -> Builder
canonicalRequest S3Request{..} =
  let
    -- We MUST sort the parameters in the query string alphabetically by key name
    qs         = sortBy (compare `on` fst) queryString
    bodyHash   = fromMaybe "UNSIGNED-PAYLOAD" payloadHash
    headers    = sortS3Headers s3headers
    headerKeys = map (fst . getS3Header) headers

    httpMethod       = renderS3Method s3method
    canonicalURI     = fromByteString objectName
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
