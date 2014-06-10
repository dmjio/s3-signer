module Network.S3.Sign  ( sign ) where

import           Crypto.Hash.SHA1       (hash)
import           Crypto.MAC.HMAC        (hmac)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as B64
import           Network.HTTP.Types.URI (urlEncode)

-- | SHA1 Encrypted Signature
sign :: ByteString -> ByteString -> ByteString
sign secretKey url = urlEncode True . B64.encode $ hmac hash 64 secretKey url
