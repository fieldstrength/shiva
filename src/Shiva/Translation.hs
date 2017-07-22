{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Shiva.Translation (
  SvenskaPair (..),
  SentencePair (..),
  runTrans,
  translateSet,
  TransArticle (..),

) where

import           Shiva.Config
-- import Shiva.Database
import           Control.Concurrent.STM.TVar
import           Control.Monad.Catch         (throwM)
import Control.Monad.Reader
import           Control.Monad.State
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Translator



data SvenskaPair = SvenskaPair
    { swedish :: Text
    , english :: Text
    , svBreaks :: [Int]
    , enBreaks :: [Int]
    }

data SentencePair = SentencePair
    { svSentence :: Text
    , enSentence :: Text
    }

mkSvenskaPair :: Text -> TransItem -> SvenskaPair
mkSvenskaPair svTxt (TransItem enTxt svB enB) = SvenskaPair svTxt enTxt svB enB

takeBetween :: Int -> Int -> Text -> Text
takeBetween start end = T.drop start . T.take end

splitter :: Text -> [Int] -> [Text]
splitter txt []       = [txt]
splitter txt (n:[])   = [T.drop n txt]
splitter txt (n:m:ns) = takeBetween n m txt : splitter txt (m:ns)


splitSentences :: SvenskaPair -> [SentencePair]
splitSentences (SvenskaPair sv en svBs enBs) =
    let svs = splitter sv svBs
        ens = splitter en enBs in
    zipWith SentencePair svs ens


translateSet :: [Text] -> ShivaM [[SentencePair]]
translateSet svTxts = do
    ShivaData {..} <- ask
    liftIO $ do
        tdata <- readTVarIO transDataTV
        ArrayResponse xs <- either throwM pure
            =<< translateArrayIO tdata Swedish English svTxts
        unless (length xs == length svTxts) $ throwM Wronglengths
        pure . map (splitSentences . uncurry mkSvenskaPair) $ zip svTxts xs



-- | Core translation function. Used only with counter machinery below.
trans :: Text -> ShivaM Text
trans sv = do
    ShivaData {..} <- ask
    tdata <- liftIO $ readTVarIO transDataTV
    mtxt <- liftIO $ translateIO tdata (Just Swedish) English sv
    either (throwM . MSTranslatorException) pure mtxt


---- Translation with running character count ----

runCounter :: CounterM a -> ShivaM a
runCounter cm = do
  (x,_) <- runStateT cm 0
  -- save translation event record in DB here
  return x

shivaTrans :: Text -> CounterM Text
shivaTrans txt = do
  let n = T.length txt
  modify (+n)
  lift $ trans txt

runTrans :: Text -> ShivaM Text
runTrans = runCounter . shivaTrans


---- Translating sets of phrases:  Used for feed listings  ----

data TransArticle = TransArticle
    { thetitle   :: Text
    , origUrl    :: Text
    , imageUrl   :: Maybe Text
    , bodyResult :: [SentencePair] }
