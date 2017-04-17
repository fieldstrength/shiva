{-# LANGUAGE RecordWildCards,
             OverloadedStrings,
             TypeApplications #-}

module Shiva.Translation (
  SvenskaPair (..),
  ShivaResult (..),
  runTrans,
  translateSet,
  translateSentences,
  prep,
  prepSep,
  barDiv,
  zipWithDefault,

  TransResult (..),
  TransArticle (..),
  translateArticleText,

) where

import Shiva.Config
-- import Shiva.Database
import Shiva.Utils               (zipWithDefault)
import Data.MonoTraversable      (omap)

import Safe                      (lastMay)
import Language.Bing             (translate, BingLanguage (..))
import Data.Text                 (Text, split)
import qualified Data.Text as T
import Control.Monad.State
import Control.Monad.Error.Class (throwError)
import Data.List                 (intersperse)
import qualified Data.ByteString.Char8 as BSC



data SvenskaPair = SvenskaPair
  { swedish :: Text
  , english :: Text }

data ShivaResult = ShivaResult
  { success :: Bool
  , result :: [SvenskaPair] }


-- | Core translation function. Used only with counter machinery below.
trans :: Text -> ShivaM Text
trans sv = do
  Config {..} <- appConfig
  mtxt <- liftIO $ translate (BSC.pack clientId) (BSC.pack clientSecret) sv Swedish English
  case mtxt of
    Left err -> throwError (show err)
    Right en -> return en


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

-- Some unnecessary complexity here. Very occassionally the translation eats our separator, '|'.
-- This results in a mismatch in the formed result. In these cases, we use punctuation to separate
-- sentences. While this may not be 100% perfect either, the rate of coincidence of these two
-- potential problems should be very small.
translateSet' :: [Text] -> ShivaM ShivaResult
translateSet' svs = do
  let pstr = prepSet svs
  en <- runTrans pstr
  let ens = barDiv en
  if length svs == length ens
    then return $ ShivaResult True $ zipWith SvenskaPair svs ens
    else let ens' = sentences $ T.unwords ens
         in  return $ ShivaResult (length svs == length ens') $
               zipWithDefault SvenskaPair "" svs ens'

translateSet :: [Text] -> ShivaM [SvenskaPair]
translateSet = fmap result . translateSet'

translateSentences :: Text -> ShivaM [SvenskaPair]
translateSentences = translateSet . prepSep



data TransResult = TransResult
  { theresult :: ShivaResult
  , svTxt :: Text
  , enTxt :: Text }

data TransArticle = TransArticle
  { thetitle :: Text
  , origUrl :: Text
  , imageUrl :: Maybe Text
  , bodyResult :: ShivaResult }


translateArticleText :: Text -> ShivaM TransResult
translateArticleText sv = do
  let psv = prep sv
  pen <- runTrans psv
  let ens = barDiv pen
      svs = barDiv psv
      ens' = sentences $ T.unwords ens
      r = if length svs == length ens
          then ShivaResult True $ zipWith SvenskaPair svs ens
          else ShivaResult (length svs == length ens') $ zipWithDefault SvenskaPair "" svs ens'
  return $ TransResult r psv pen



---- Preparation I: Prepare a list of phrases for translation ----


---- Intersperse '|' between phrases ----

prepSet :: [Text] -> Text
prepSet = mconcat . intersperse " | " . map (omap $ replaceElem '|' '~')

replace :: Char -> Char -> Text -> Text
replace x y = omap $ replaceElem x y

replaceElem :: Eq a => a -> a -> a -> a
replaceElem x y z = if z == x then y else z

---- Inverse of prepartion: Separate on every occurrence of '|' ----

barDiv :: Text -> [Text]
barDiv = split (=='|')

barJoin :: [Text] -> Text
barJoin = mconcat . intersperse " | "


---- Preparation II: Prepare a body of text, separating into sentences ----

-- | Remove any current instances of the '|' character. Then separate into sentences and
--   delineate them with |.
prepSep :: Text -> [Text]
prepSep = sentences . replace '|' '~'

-- | Remove any current instances of the '|' character. Then separate into sentences and
--   delineate them with |.
prep :: Text -> Text
prep = barJoin . prepSep

---- Separating by sentence ----

sentences :: Text -> [Text]
sentences = map T.unwords . sepOn punctuated . T.words

punctuated :: Text -> Bool
punctuated = mayTest (flip (elem @[]) punctuation) . lastMay . T.unpack
  where mayTest :: (Char -> Bool) -> Maybe Char -> Bool
        mayTest p (Just x) = p x
        mayTest _ Nothing  = False

        punctuation = ".!?"

-- Separate list at each occurrence of the predicate-satisfying element.
sepOn :: (a -> Bool) -> [a] -> [[a]]
sepOn p l = case sep p l of
  (xs,[]) -> [xs]
  (xs,ys) -> xs : sepOn p ys

-- Similar to prelude function 'break' except we need to keep predicate-satisfying elements on
-- the end of the first list rather than the start of the second.
sep :: (a -> Bool) -> [a] -> ([a],[a])
sep q l = runSep q ([],l)
  where runSep _ (xs,[])   = (xs,[])
        runSep p (xs,y:ys) = let z = (xs++[y],ys)
                             in if p y then z else runSep p z


-- https://datamarket.azure.com/developer/applications/
-- https://www.microsoft.com/en-us/translator/getstarted.aspx
