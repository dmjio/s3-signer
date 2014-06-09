module Network.S3.Sign  ( sign  ) where

import qualified Data.ByteString.Base64.Lazy as B64
import           Data.ByteString.Lazy.UTF8   (ByteString)
import           Data.Digest.Pure.SHA        (bytestringDigest, hmacSha1)
import           Network.S3.Util             (encodeURL)

-- | SHA1 Encrypted Signature
sign :: ByteString -> ByteString -> ByteString
sign secretKey url =
    encodeURL . B64.encode . bytestringDigest $
           hmacSha1 secretKey url
