{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.S3.Sign  ( 
    sign 
  , algorithm
  , mkScope
  , mkTimestamp
) where

import qualified    Data.ByteString.Char8    as BS
import qualified    Data.ByteString.Base64   as Base64
import              Data.ByteString.UTF8     (ByteString)
import              Blaze.ByteString.Builder (toByteString, fromByteString, Builder)
import              Data.Monoid              ((<>))
import              Data.Time.Format         (formatTime, defaultTimeLocale)
import              Network.S3.Types         (S3Request(..), S3SignedRequest(..))
import              Network.S3.URL           (canonicalRequest)
import              Data.Byteable            (toBytes)
import              Crypto.Hash

reqString :: S3Request -> Digest SHA256
reqString = hash . toByteString . canonicalRequest

hmacSHA256 :: ByteString -> ByteString -> HMAC SHA256
hmacSHA256 key = hmac key

hmac_ :: ByteString -> ByteString -> ByteString
hmac_ key = toBytes . hmacGetDigest . hmacSHA256 key


mkScope :: S3Request -> Builder
mkScope req = 
  let
    date      = BS.pack (formatTime defaultTimeLocale "%Y%m%d" (requestTime req))
    region    = regionName req
    service   = "s3"
  in
    fromByteString date   <> fromByteString "/" <>
    fromByteString region <> fromByteString "/" <>
    fromByteString service <> fromByteString "/aws4_request"

algorithm :: Builder
algorithm = "AWS4-HMAC-SHA256"

mkTimestamp :: S3Request -> Builder
mkTimestamp req = 
  let
    seconds   = BS.pack (formatTime defaultTimeLocale "T%H%M%SZ" (requestTime req))
    date      = BS.pack (formatTime defaultTimeLocale "%Y%m%d" (requestTime req))
  in
    fromByteString (date <> seconds)

-- | SHA1 Encrypted Signature
sign :: S3Request -> S3SignedRequest
sign req =
  let
      date      = BS.pack (formatTime defaultTimeLocale "%Y%m%d" (requestTime req))
      timestamp = mkTimestamp req
      reqHash   = fromByteString (digestToHexByteString (reqString req))
      region    = regionName req
      service   = "s3"

      dateKey              = hmac_ ("AWS4" <> s3Secret req) date
      dateRegionKey        = hmac_ dateKey region
      dateRegionServiceKey = hmac_ dateRegionKey service
      signingKey           = hmac_ dateRegionServiceKey "aws4_request"

      scope = mkScope req

      signingStringBuilder =
        algorithm <> "\n" <>
        timestamp <> "\n" <>
        scope     <> "\n" <>
        reqHash

      stringToSign = toByteString signingStringBuilder
      signature    = hmacSHA256 signingKey stringToSign
      hexSignature = digestToHexByteString (hmacGetDigest signature)
  in
    S3SignedRequest {
        sigHeaders    = s3headers req
      , sigDate       = toByteString timestamp
      , sigCredential = toByteString scope
      , sigPolicy     = Base64.encode stringToSign
      , sigSignature  = hexSignature
      , sigAlgorithm  = toByteString algorithm
    }
