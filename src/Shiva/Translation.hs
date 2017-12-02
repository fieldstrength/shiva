{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Shiva.Translation (
    runTrans,
    translateSet,
    translateSentences,
    TransArticle (..),

) where

import           Shiva.Config

import           Control.Concurrent.STM.TVar
import           Control.Monad.Catch                  (throwM)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Database.PostgreSQL.Simple.FromField
import           GHC.Generics
import           Opaleye
import           Translator


translateSentences :: [Text] -> ShivaM [[Sentence]]
translateSentences svTxts = do
    ShivaData {..} <- ask
    liftIO $ do
        tdata <- readTVarIO transDataTV
        translateArraySentencesIO tdata Swedish English svTxts
            >>= either throwM pure

translateSet :: [Text] -> ShivaM [TransItem]
translateSet svTxts = do
    ShivaData {..} <- ask
    liftIO $ do
        tdata <- readTVarIO transDataTV
        translateArrayIO tdata Swedish English svTxts
            >>= either throwM (pure . getArrayResponse)


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

deriving instance ToJSON   Sentence
deriving instance FromJSON Sentence

data TransArticle = TransArticle
    { thetitle    :: Text
    , origUrl     :: Text
    , urlFragment :: Text
    , imageUrl    :: Maybe Text
    , bodyResult  :: [Sentence]
    } deriving (Show, Generic, FromJSON, ToJSON)

instance FromField TransArticle where
    fromField = fromJSONField

instance QueryRunnerColumnDefault PGJsonb TransArticle where
    queryRunnerColumnDefault = fieldQueryRunnerColumn
