{-# LANGUAGE RecordWildCards #-}

module Shiva.Translation (
  SvenskaPair (..),
  ShivaResult (..),
  runTrans,
  translateSet,
  translateSentences,
  prep,
  prepSep,
  divOn,
  zipWithDefault,

  TransResult (..),
  TransArticle (..),
  translateArticleText,

) where

import Shiva.Config
import Shiva.Database
import Shiva.Utils               (zipWithDefault)

import Safe                      (lastMay)
import Language.Bing             (translate, BingLanguage (..))
import Data.Text                 (Text,pack,unpack)
import qualified Data.Text as T
import Control.Monad.State
import Control.Monad.Error.Class (throwError)
import Data.List                 (intercalate)
import qualified Data.ByteString.Char8 as BSC



data SvenskaPair = SvenskaPair
  { swedish :: String
  , english :: String }

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

bumpCount :: Int -> ShivaM ()
bumpCount n = do
  s <- readCharCount
  writeCharCount (s+n)

runCounter :: CounterM a -> ShivaM a
runCounter cm = do
  (x,s) <- runStateT cm 0
  bumpCount s
  return x

shivaTrans :: Text -> CounterM Text
shivaTrans txt = do
  let n = T.length txt
  modify (+n)
  lift $ trans txt

runTrans :: String -> ShivaM String
runTrans = fmap unpack . runCounter . shivaTrans . pack


---- Translating sets of phrases:  Used for feed listings  ----

-- Some unnecessary complexity here. Very occassionally the translation eats our separator, '|'.
-- This results in a mismatch in the formed result. In these cases, we use punctuation to separate
-- sentences. While this may not be 100% perfect either, the rate of coincidence of these two
-- potential problems should be very small.
translateSet' :: [String] -> ShivaM ShivaResult
translateSet' svs = do
  let pstr = prepSet svs
  en <- runTrans pstr
  let ens = divOn '|' en
  if length svs == length ens
    then return $ ShivaResult True $ zipWith SvenskaPair svs ens
    else let ens' = sentences $ unwords ens
         in  return $ ShivaResult (length svs == length ens') $
               zipWithDefault SvenskaPair "" svs ens'

translateSet :: [String] -> ShivaM [SvenskaPair]
translateSet = fmap result . translateSet'

translateSentences :: String -> ShivaM [SvenskaPair]
translateSentences = translateSet . prepSep



data TransResult = TransResult
  { theresult :: ShivaResult
  , svTxt :: String
  , enTxt :: String }

data TransArticle = TransArticle
  { thetitle :: String
  , origUrl :: String
  , imageUrl :: Maybe String
  , bodyResult :: ShivaResult }


translateArticleText :: String -> ShivaM TransResult
translateArticleText sv = do
  let psv = prep sv
  pen <- runTrans psv
  let ens = divOn '|' pen
      svs = divOn '|' psv
      ens' = sentences $ unwords ens
      r = if length svs == length ens
          then ShivaResult True $ zipWith SvenskaPair svs ens
          else ShivaResult (length svs == length ens') $ zipWithDefault SvenskaPair "" svs ens'
  return $ TransResult r psv pen





---- Preparation I: Prepare a list of phrases for translation ----


---- Intersperse '|' between phrases ----

prepSet :: [String] -> String
prepSet = intercalate " | " . map (replace '|' '~')

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map $ \i -> if i == x then y else i


---- Inverse of prepartion: Separate on every occurrence of '|' ----

-- Separate list on each appearance of the special element.
divOn :: Eq a => a -> [a] -> [[a]]
divOn x l = case sep' x l of
  (xs,[]) -> [xs]
  (xs,ys) -> xs : divOn x ys

-- The list of elements occuring before the (first) designated element, and those occurring after.
sep' :: Eq a => a -> [a] -> ([a],[a])
sep' x l = runSep x ([],l)
  where runSep _ (xs,[])   = (reverse xs,[])
        runSep y (xs,z:zs) = if y == z then (reverse xs,zs) else runSep y (z:xs,zs)


---- Preparation II: Prepare a body of text, separating into sentences ----

-- | Remove any current instances of the '|' character. Then separate into sentences and
--   delineate them with |.
prep :: String -> String
prep = intercalate " | " . sentences . replace '|' '~'

-- | Remove any current instances of the '|' character. Then separate into sentences and
--   delineate them with |.
prepSep :: String -> [String]
prepSep = sentences . replace '|' '~'

---- Separating by sentence ----

sentences :: String -> [String]
sentences = map unwords . sepOn punctuated . words

punctuated :: String -> Bool
punctuated = mayTest (`elem` punctuation) . lastMay
  where mayTest p (Just x) = p x
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
