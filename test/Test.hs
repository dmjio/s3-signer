{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (putStrLn)

import Blaze.ByteString.Builder (toByteString)
import Data.ByteString.Char8 (putStrLn)
import Data.ByteString (ByteString)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (getCurrentTime, UTCTime(..))
import Network.S3
import Network.S3.URL (canonicalRequest)
import Network.S3.Sign (sign)

main :: IO ()
main = do
  let time = UTCTime (fromGregorian 2013 5 24) 0
  print $ sign privKey (exampleRequest time) time

privKey :: ByteString
privKey = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"

pubKey :: ByteString
pubKey = "AKIAIOSFODNN7EXAMPLE"

-- https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html#example-signature-GET-object
exampleRequest :: UTCTime -> S3Request
exampleRequest time =
  S3Request {
      s3method    = S3GET
    , mimeType    = Nothing
    , bucketName  = "examplebucket"
    , regionName  = "us-east-1"
    , objectName  = "test.txt"
    , queryString = []
    , payloadHash = Nothing
    , requestTime = time
    , s3headers   = [ s3Header "range" "bytes=0-9" ]
  }


