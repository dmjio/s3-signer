{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Main where

import Data.ByteString (ByteString)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import Network.S3
import Control.Monad (unless)
import Network.S3.Sign (sign)
import System.Exit (exitFailure)

expectedSig :: ByteString
expectedSig = "f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41"

testFail :: String -> IO ()
testFail msg = do
  print msg
  exitFailure

assertEqual :: (Show a, Eq a) => a -> a -> IO ()
assertEqual got expected = do
  let msg = "'" ++ show got ++ " does not match expected ''" ++ "'"
  unless (got == expected) (testFail msg)

main :: IO ()
main = do
  let time = UTCTime (fromGregorian 2013 5 24) 0
      sr@S3SignedRequest{..} = sign privKey (exampleRequest time) time
  assertEqual sigSignature expectedSig
  print sr

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


