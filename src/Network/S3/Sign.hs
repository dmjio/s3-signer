{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.S3.Sign  ( sign ) where

import qualified    Data.ByteString.Char8    as BS
import qualified    Data.ByteString.Base64   as Base64
import              Data.ByteString.UTF8     (ByteString)
import              Blaze.ByteString.Builder (toByteString, fromByteString)
import              Data.Monoid              ((<>))
import              Data.Time.Clock          (UTCTime)
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


-- | SHA1 Encrypted Signature
sign :: ByteString -> S3Request -> UTCTime -> S3SignedRequest
sign key req utc =
  let
      seconds   = BS.pack (formatTime defaultTimeLocale "T%H%M%SZ" utc)
      date      = BS.pack (formatTime defaultTimeLocale "%Y%m%d" utc)

      timestamp = fromByteString (date <> seconds)
      reqHash   = fromByteString (digestToHexByteString (reqString req))
      region    = regionName req
      service   = "s3"

      dateKey              = hmac_ ("AWS4" <> key) date
      dateRegionKey        = hmac_ dateKey region
      dateRegionServiceKey = hmac_ dateRegionKey service
      signingKey           = hmac_ dateRegionServiceKey "aws4_request"

      scope =
        fromByteString date   <> fromByteString "/" <>
        fromByteString region <> fromByteString "/" <>
        fromByteString service <> fromByteString "/aws4_request"

      algorithm = "AWS4-HMAC-SHA256"

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
