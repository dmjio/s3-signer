{-# LANGUAGE DeriveGeneric #-}

module Network.S3.Types
    ( S3URL (..)
    , S3Creds (..)
    , S3RequestType (..)
    , S3Request (..)
    ) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)

newtype S3URL = S3URL {
      signedRequest :: Text
    } deriving (Generic, Show)

data S3Creds = S3Creds {
      publicKey :: ByteString
    , secretKey :: ByteString
    } deriving (Generic, Show)

data S3RequestType = S3GET | S3PUT
    deriving (Generic, Show)

data S3Request = S3Request {
      requestType     :: S3RequestType
    , bucketName      :: ByteString
    , objectName      :: ByteString
    , secondsToExpire :: Integer
    } deriving (Generic, Show)
