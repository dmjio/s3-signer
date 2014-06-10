s3-signer
======
s3-signer is intended to be an aid in building secure cloud-based services with
AWS. This library generates cryptographically secure URL's that
expire at a user-defined interval. These URLs can be used to offload
the process of uploading an downloading large files, freeing your
webserver to focus on other things. 

### Features
 - Minimal depedencies
 - Web framework agnostic
 - Reduced web server load
 - Ideal for AJAX direct-to-s3 upload scenarios

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

-- | HMAC-SHA1 Encrypted Signature
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
type FileID = ByteString

makeS3URL :: FileID -> IO S3URL
makeS3URL fileId = generateS3URL credentials request
  where
    credentials = S3Keys "<public-key-goes-here>" "<secret-key-goes-here>"
    request     = S3Request S3GET "bucket-name" (fileId <> ".zip") 3 

downloadFile :: Handler App (AuthManager App) ()
downloadFile = method POST $ currentUserId >>= maybe the404 handleDownload
  where handleDownload _ = do
          Just fileId <- getParam "fileId"
          S3URL url <- liftIO $ makeS3URL fileId
          redirect' url 302
```
### Direct to S3 AJAX Uploads
   - Configure S3 Bucket CORS Policy settings
   - [CORS Docs](http://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html#how-do-i-enable-cors)

```xml
<?xml version="1.0" encoding="UTF-8"?>
<CORSConfiguration xmlns="http://s3.amazonaws.com/doc/2006-03-01/">
    <CORSRule>
        <AllowedOrigin>https://my-url-goes-here.com</AllowedOrigin>
        <AllowedMethod>PUT</AllowedMethod>
        <AllowedHeader>*</AllowedHeader>
    </CORSRule>
</CORSConfiguration>
```
   - Retrieve PUT Request URL via AJAX 

```haskell
type FileID = ByteString

makeS3URL :: FileID -> IO S3URL
makeS3URL fileId = generateS3URL credentials request
  where
    credentials = S3Keys "<public-key-goes-here>" "<secret-key-goes-here>"
    request     = S3Request S3PUT "bucket-name" (fileId <> ".zip") 3 

instance ToJSON S3URL

getUploadURL :: Handler App (AuthManager App) ()
getUploadURL = method POST $ currentUserId >>= maybe the404 handleDownload
  where handleDownload _ = do
          Just fileId <- getParam "fileId"
          writeJSON =<< liftIO (makeS3URL fileId)
```
   - Embed FileReader blob data to request
   - Send upload request

```javascript
var xhr = new XMLHttpRequest();
xhr.open('PUT', url /* S3-URL generated from server */);
xhr.setRequestHeader('Content-Type', 'application/zip'); /* whatever http-content-type makes sense */
xhr.setRequestHeader('x-amz-acl', 'public-read');

/* upload completion check */
xhr.onreadystatechange = function(e) {
    if (this.readyState === 4 && this.status === 200) 
          console.log('upload complete');
};

/* Amazon gives you progress information on AJAX Uploads */
xhr.upload.addEventListener("progress", function(evt) {
       if (evt.lengthComputable) {
          var v = (evt.loaded / evt.total) * 100,
          val = Math.round(v) + '%',
          console.log('Completed: ' + val);
      }
}, false);

/* error handling */
xhr.upload.addEventListener("error", function(evt) {
   console.log("There has been an error :(");
}, false);

/* Commence upload */
xhr.send(file); // file here is a blob from the file reader API
```
### File Reader Info
[How to read file data from the browser](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)

### Troubleshoooting
- Why do I keep getting 403 forbidden when I attempt to upload or  download from a pre-signed URL?
  * Ask yourself the following:
    - Are my keys specified correctly?
    - Did I configure the CORS settings on my bucket properly?
- Why are my URLs expiring faster than the specified time?
  * Ask yourself the following:
    - Is my server's clock synchronized with AWS? [See wiki for NTP info](https://github.com/dmjio/s3-signer/wiki/If-URLs-expire-too-quickly)

### FAQ
- Why didn't you use HMAC-SHA256?
  * It's 30% slower, and no less secure than HMAC-SHA1
  


