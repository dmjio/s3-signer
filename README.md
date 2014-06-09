## s3-signer

### Features
 - Minimal depedencies
 - Web Framework agnostic
 - Reduced Web Server load
 - Great for AJAX direct-to-s3 upload scenarios

[S3 Query String Request Authentication](http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html#RESTAuthenticationQueryStringAuth)

### Implementation

> AWS Specification

```shell
Signature = URL-Encode( Base64( HMAC-SHA1( YourSecretAccessKeyID,UTF-8-Encoding-Of( StringToSign ) ) ) );
```
> Haskell Implementation

```haskell
module Network.S3.Sign  ( sign ) where

import qualified Data.ByteString.Base64.Lazy as B64
import           Data.ByteString.Lazy.UTF8   (ByteString)
import           Data.Digest.Pure.SHA        (bytestringDigest, hmacSha1)
import           Network.S3.Util             (encodeURL)

-- | SHA1 Encrypted Signature
sign :: ByteString -> ByteString -> ByteString
sign secretKey url = encodeURL . B64.encode . bytestringDigest $ hmacSha1 secretKey url
```

### Use case:
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.S3

main :: IO ()
main = print =<< generateS3URL credentials request
  where
     credentials = S3Creds "<public-key-goes-here>" "<secret-key-goes-here>"
     request     = S3Request S3GET "bucket-name" "file-name.extension" 3
```
### Result:
```haskell
S3URL {
      signedRequest =
         "https://bucket-name.s3.amazonaws.com/file-name.extension?AWSAccessKeyId=<public-key-goes-here>&Expires=1402346638&Signature=1XraY%2Bhp117I5CTKNKPc6%2BiihRA%3D"
     }
```

### Snap integration - downloads
```haskell
-- Quick and dirty example
getDownloadUrl :: Handler App (AuthManager App) ()
getDownloadUrl = method POST $ currentUserId >>= maybe the404 handleDownload
  where handleDownload _ = do
          S3URL url <- liftIO makeS3URL 
          redirect' (encodeUtf8 url) 302
        makeS3URL   = generateS3URL credentials request
        credentials = S3Creds "<public-key-goes-here>" "<secret-key-goes-here>"
        request     = S3Request S3GET "bucket-name" "file-name.extension" 3
```
### Direct to S3 AJAX Uploads
... examples in the works
