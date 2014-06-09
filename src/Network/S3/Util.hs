module Network.S3.Util
    ( encodeURL
    ) where

import qualified Data.ByteString        as B
import           Data.ByteString.Char8  (ByteString)
import qualified Data.ByteString.Lazy   as BL
import           Network.HTTP.Types.URI (urlEncode)

strictToLazy :: ByteString -> BL.ByteString
strictToLazy = BL.fromChunks . (:[])

lazyToStrict :: BL.ByteString -> ByteString
lazyToStrict = B.concat . BL.toChunks

encodeURL :: BL.ByteString -> BL.ByteString
encodeURL = strictToLazy . urlEncode True . lazyToStrict
