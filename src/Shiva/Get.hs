-- | Http GET requests with exceptions handled in the preferred way.
module Shiva.Get (httpGet) where

import Shiva.Config         (IOX)

import Control.Monad.Except (ExceptT (..))
import Network.HTTP.Conduit (simpleHttp, HttpException)
import Control.Exception    (catch)
import Data.Text            (Text, unpack)
import Data.Text.Encoding   (decodeUtf8')
import Data.ByteString.Lazy (toStrict)
import Data.ByteString      (ByteString)
import Data.Bifunctor       (first)

reportHttpException :: HttpException -> IO (Either String a)
reportHttpException = return . Left . show

safeDecode :: ByteString -> Either String Text
safeDecode = first show . decodeUtf8'

httpGet :: Text -> IOX Text
httpGet url = ExceptT $ do
  mbs <- fmap Right (simpleHttp $ unpack url) `catch` reportHttpException
  return $ safeDecode . toStrict =<< mbs
