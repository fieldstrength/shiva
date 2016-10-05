{-# LANGUAGE OverloadedStrings #-}

-- | For extracting article content from HTML.
module Shiva.Extract (
  extractDivId,
  extractDN,
) where

import Text.HTML.TagSoup
import Text.StringLike (StringLike)
import Text.HTML.TagSoup.Match
import Prelude hiding (writeFile,readFile)


isDivOpen :: StringLike a => Tag a -> Bool
isDivOpen = tagOpen (=="div") (const True)


isDivClose :: StringLike a => Tag a -> Bool
isDivClose = tagClose (=="div")

noScript :: StringLike a => [Tag a] -> [Tag a]
noScript [] = []
noScript (t:ts) | tagOpen (=="script") (const True) t = dropScript ts
                | otherwise = t : noScript ts
  where dropScript :: StringLike a => [Tag a] -> [Tag a]
        dropScript = snd . sep (tagClose (=="script"))

tail_ :: [a] -> [a]
tail_ [] = []
tail_ (_:xs) = xs

divOpenWithId :: StringLike a => a -> Tag a -> Bool
divOpenWithId i = tagOpen (=="div") (elem ("class",i))


takeContentTags :: StringLike a => Int -> [Tag a] -> [Tag a]
takeContentTags 0 _      = []
takeContentTags _ []     = []
takeContentTags n (t:ts) | isDivOpen  t = takeContentTags (n+1) ts
                         | isDivClose t = takeContentTags (n-1) ts
                         | otherwise    = t : takeContentTags n ts


extractDivIdTags :: StringLike a => a -> [Tag a] -> [Tag a]
extractDivIdTags i = takeContentTags 1 . noScript . tail_ . dropWhile (not . divOpenWithId i)

extractDivId :: StringLike a => a -> a -> a
extractDivId i = innerText . extractDivIdTags i . parseTags

extractDN :: StringLike a => a -> a
extractDN = extractDivId "article__body-content"

sep :: (a -> Bool) -> [a] -> ([a],[a])
sep q l = runSep q ([],l)
  where runSep _ (xs,[])   = (reverse xs,[])
        runSep p (xs,y:ys) = if p y then (reverse xs,ys) else runSep p (y:xs,ys)


{-
parseTags :: StringLike str => str -> [Tag str]
-- | Extract all text content from tags (similar to Verbatim found in HaXml)
innerText :: StringLike str => [Tag str] -> str
innerText = strConcat . mapMaybe maybeTagText

-- | Extract the string from within 'TagText', otherwise 'Nothing'
maybeTagText :: Tag str -> Maybe str
maybeTagText (TagText x) = Just x
maybeTagText _           = Nothing

getTagContent :: Eq str => str -> ([Attribute str] -> Bool) -> [Tag str] -> [Tag str]
-}
