{-# LANGUAGE OverloadedStrings #-}

module Network.S3.URL
    ( putURL
    , getURL
    , s3URL
    ) where

import           Data.Monoid
import           Data.String

-- | S3 Upload URL Template
putURL :: (Monoid m, IsString m) => m -> m -> m -> m -> m
putURL bucket object expires mimetype =
    mconcat [ "PUT\n\n"
            , mimetype 
            ,"\n"
            , expires
            , "\nx-amz-acl:public-read\n/"
            , bucket
            , "/"
            , object
            ]

-- | S3 Download URL Template
getURL :: (Monoid m, IsString m) => m -> m -> m -> m
getURL bucket object expires =
    mconcat [ "GET\n\n\n"
            , expires
            , "\n/"
            , bucket
            , "/"
            , object
            ]

-- | Amazon S3 URL Template
baseUrl :: (Monoid m, IsString m) => m -> m -> m
baseUrl bucket object =
    mconcat [ "https://"
            , bucket
            , ".s3.amazonaws.com/"
            , object
            ]

s3URL :: (Monoid m, IsString m) =>
           m -> m -> m -> m -> m -> m
s3URL bucket object publicKey expires sig
    = mconcat [ baseUrl bucket object
              , "?AWSAccessKeyId="
              , publicKey
              , "&Expires="
              , expires
              , "&Signature="
              , sig
              ]
