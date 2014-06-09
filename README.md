## s3-signer

### Features
 - Minimal depedencies
 - Web Framework agnostic
 - Reduces web server load
 - Great for AJAX direct-to-s3 upload scenarios

[S3 Query String Request Authentication](http://docs.aws.amazon.com/AmazonS3/latest/dev/RESTAuthentication.html#RESTAuthenticationQueryStringAuth)

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
