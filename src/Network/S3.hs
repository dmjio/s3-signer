{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.S3
    ( -- * Create pre-signed AWS S3 URL
      generateS3URL
      -- * Types
    , module Network.S3.Types
    , module Network.S3.URL
    ) where

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.List (intersperse)
import           Data.Maybe (fromMaybe)
import           Network.S3.Sign
import           Network.S3.Types
import           Network.S3.URL
import           Blaze.ByteString.Builder (toByteString, fromByteString)
import qualified URI.ByteString as URI


-- | Build a canonical request to be signed along with the URI containing
-- all the headers in the query string
generateS3URL :: S3Request -> ByteString
generateS3URL req@S3Request{..} =
  let 
      headers = s3Header "host" host : s3headers
      headerKeys = map (fst . getS3Header) headers
      signedHeaders = foldMap fromByteString (intersperse ";" headerKeys)

      qs = [
          ("X-Amz-Algorithm",  Just (toByteString algorithm))
        , ("X-Amz-Credential", Just (toByteString (fromByteString s3Key <> "/" <> mkScope req)))
        , ("X-Amz-SignedHeaders", Just (toByteString signedHeaders))
        , ("X-Amz-Date", Just (toByteString (mkTimestamp req)))
        , ("X-Amz-Expires", Just (pack (show expires)))
        ] ++ queryString
      host = "s3-" <> regionName <> ".amazonaws.com"
      fullObject = bucketName <> "/" <> objectName
      signRes = sign req{
          queryString = qs
        , s3headers = headers
        , objectName = fullObject
      }
      uri = URI.URI (URI.Scheme "https")
              (Just (URI.Authority Nothing (URI.Host host) Nothing))
              ("/" <> fullObject)
              (URI.Query (("X-Amz-Signature", sigSignature signRes) 
                          : (fmap (fromMaybe "") <$> qs)))
              Nothing
  in
    URI.serializeURIRef' uri
