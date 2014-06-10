## s3-signer

### Features
 - Minimal depedencies
 - Web Framework agnostic
 - Reduced Web Server load
 - Great for AJAX direct-to-s3 upload scenarios

### Documentation
[S3 Query String Request Authentication](http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html#RESTAuthenticationQueryStringAuth)

### Implementation

> AWS Specification

```shell
Signature = URL-Encode( Base64( HMAC-SHA1( YourSecretAccessKeyID,UTF-8-Encoding-Of( StringToSign ) ) ) );
```
> Haskell Implementation

```haskell
module Network.S3.Sign  ( sign ) where

import           Crypto.Hash.SHA1       (hash)
import           Crypto.MAC.HMAC        (hmac)
import qualified Data.ByteString.Base64 as B64
import           Data.ByteString.UTF8   (ByteString)
import           Network.HTTP.Types.URI (urlEncode)

-- | SHA1 Encrypted Signature
sign :: ByteString -> ByteString -> ByteString
sign secretKey url = urlEncode True . B64.encode $ hmac hash 64 secretKey url
```

### Use Case
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.S3

main :: IO ()
main = print =<< generateS3URL credentials request
  where
     credentials = S3Keys "<public-key-goes-here>" "<secret-key-goes-here>"
     request     = S3Request S3GET "bucket-name" "file-name.extension" 3 -- 3 secs until expired
```
### Result
```haskell
S3URL {
      signedRequest =
         "https://bucket-name.s3.amazonaws.com/file-name.extension?AWSAccessKeyId=<public-key-goes-here>&Expires=1402346638&Signature=1XraY%2Bhp117I5CTKNKPc6%2BiihRA%3D"
     }
```

### Snap integration - Downloads
```haskell
-- Quick and dirty example
getDownloadUrl :: Handler App (AuthManager App) ()
getDownloadUrl = method POST $ currentUserId >>= maybe the404 handleDownload
  where handleDownload _ = do
          S3URL url <- liftIO makeS3URL 
          redirect' url 302
        makeS3URL   = generateS3URL credentials request
        credentials = S3Keys "<public-key-goes-here>" "<secret-key-goes-here>"
        request     = S3Request S3GET "bucket-name" "file-name.extension" 3
```
### Direct to S3 AJAX Uploads
... examples in the works
