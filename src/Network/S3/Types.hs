{-# LANGUAGE DeriveGeneric #-}

module Network.S3.Types
    ( S3URL (..)
    , S3Keys (..)
    , S3Method (..)
    , S3Request (..)
    ) where

import           Data.ByteString (ByteString)
import           GHC.Generics    (Generic)

-- | Result
newtype S3URL = S3URL {
      signedRequest :: ByteString -- ^ The resultant URL
    } deriving (Generic, Show)

data S3Keys = S3Keys {
      publicKey :: ByteString -- ^ AWS Public Key
    , secretKey :: ByteString  -- ^ AWS Private Key
    } deriving (Generic, Show)

data S3Method = S3GET -- ^ GET Request
              | S3PUT -- ^ PUT Request
    deriving (Generic, Show)

data S3Request = S3Request {
      s3method        :: S3Method -- ^ Type of HTTP Method
    , bucketName      :: ByteString -- ^ Name of Amazon S3 Bucket
    , objectName      :: ByteString -- ^ Name of Amazon S3 File
    , secondsToExpire :: Integer -- ^ Each whole number is a second
    } deriving (Generic, Show)
