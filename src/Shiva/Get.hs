-- | Http GET requests with exceptions handled in the preferred way.
module Shiva.Get (httpGet) where

import Shiva.Config (IOX)

import Control.Monad.Except (ExceptT (..))
import Network.HTTP.Conduit (simpleHttp, HttpException)
import Control.Exception    (catch)
import Data.Text            (Text)
import Data.Text.Encoding   (decodeUtf8)
import Data.ByteString.Lazy (toStrict)

reportHttpException :: HttpException -> IO (Either String a)
reportHttpException = return . Left . show

httpGet :: String -> IOX Text
httpGet url = ExceptT $ do
  bs <- fmap Right (simpleHttp url) `catch` reportHttpException
  return $ decodeUtf8 . toStrict <$> bs
