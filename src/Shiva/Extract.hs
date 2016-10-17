{-# LANGUAGE OverloadedStrings #-}

-- | For extracting article content from HTML.
module Shiva.Extract (
  extractDivId,
  extractDivsWithIds
) where

import Text.HTML.TagSoup
import Text.StringLike (StringLike,strConcat)
import Text.HTML.TagSoup.Match
import Prelude hiding (writeFile,readFile)


----- Removing script tags -----

dropScript :: StringLike a => [Tag a] -> [Tag a]
dropScript [] = []
dropScript ts = before ++ (dropScript . snd . sepClosed) after
  where sepOpen = sep $ tagOpenNameLit "script"
        sepClosed = sep $ tagCloseNameLit "script"
        (before,after) = sepOpen ts

-- Separate list into elements before and after the first predicate-satisfying element
sep :: (a -> Bool) -> [a] -> ([a],[a])
sep q l = runSep q ([],l)
  where runSep _ (xs,[])   = (reverse xs,[])
        runSep p (xs,y:ys) = if p y then (reverse xs,ys) else runSep p (y:xs,ys)


----- Extract from divs with specified Ids -----


isDivOpen :: StringLike a => Tag a -> Bool
isDivOpen = tagOpen (=="div") (const True)

isDivClose :: StringLike a => Tag a -> Bool
isDivClose = tagClose (=="div")

takeContentTags :: StringLike a => Int -> [Tag a] -> [Tag a]
takeContentTags 0 _      = []
takeContentTags _ []     = []
takeContentTags n (t:ts) | isDivOpen  t = takeContentTags (n+1) ts
                         | isDivClose t = takeContentTags (n-1) ts
                         | otherwise    = t : takeContentTags n ts


-----


tail_ :: [a] -> [a]
tail_ [] = []
tail_ (_:xs) = xs

divOpenWithId :: StringLike a => a -> Tag a -> Bool
divOpenWithId i = tagOpenAttrLit "div" ("class",i)

extractDivId :: StringLike a => a -> a -> a
extractDivId i = innerText
               . takeContentTags 1
               . dropScript
               . tail_
               . dropWhile (not . divOpenWithId i)
               . parseTags

-----

prepTags :: StringLike a => a -> [Tag a]
prepTags = dropScript . parseTags

extractDivTextWithId :: StringLike a => a -> [Tag a] -> a
extractDivTextWithId i = innerText
                       . takeContentTags 1
                       . tail_
                       . dropWhile (not . divOpenWithId i)

extractDivsWithIds :: StringLike a => [a] -> a -> a
extractDivsWithIds ids str = strConcat $ extractDivTextWithId <$> ids <*> [prepTags str]




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
