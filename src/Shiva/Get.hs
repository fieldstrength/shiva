-- | Http GET requests with exceptions handled in the preferred way.
module Shiva.Get (httpGet) where

import           Shiva.Config         (ShivaException (..))

import           Control.Monad.Catch  (throwM, try)
import           Data.ByteString.Lazy (toStrict)
import           Data.Text            (Text, unpack)
import           Data.Text.Encoding   (decodeUtf8')
import           Network.HTTP.Conduit (simpleHttp)

httpGet :: Text -> IO Text
httpGet url = do
  mbs <- try . simpleHttp $ unpack url
  bs <- either (throwM . NetworkException) pure mbs
  either (throwM . TextException) pure $ decodeUtf8' (toStrict bs)
