{-# LANGUAGE OverloadedStrings #-}

-- | For extracting article content from HTML.
module Shiva.Extract (
  extractDivClass,
  extractDivText,
  extractImgUrl,
  extractImgUrl_noParams,
) where

import Data.Sequences          (takeWhile)
import Data.Text               (Text)
import Prelude                 hiding (readFile, takeWhile, writeFile)
import Safe                    (headMay, tailSafe)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.StringLike         (StringLike, strConcat)



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
    where
        runSep _ (xs,[])   = (reverse xs,[])
        runSep p (xs,y:ys) = if p y then (reverse xs,ys) else runSep p (y:xs,ys)


----- Extract from divs with specified Ids -----


isDivOpen :: StringLike a => Tag a -> Bool
isDivOpen = tagOpenLit "div" (const True)

isDivClose :: StringLike a => Tag a -> Bool
isDivClose = tagCloseLit "div"

takeContentTags :: StringLike a => Int -> [Tag a] -> [Tag a]
takeContentTags 0 _      = []
takeContentTags _ []     = []
takeContentTags n (t:ts) | isDivOpen  t = takeContentTags (n+1) ts
                         | isDivClose t = takeContentTags (n-1) ts
                         | otherwise    = t : takeContentTags n ts


-----

divOpenWithClass :: StringLike a => a -> Tag a -> Bool
divOpenWithClass i = tagOpenAttrLit "div" ("class",i)

extractDivClass :: StringLike a => a -> a -> a
extractDivClass i
    = innerText
    . takeContentTags 1
    . dropScript
    . tailSafe
    . dropWhile (not . divOpenWithClass i)
    . parseTags

-----

prepTags :: StringLike a => a -> [Tag a]
prepTags = dropScript . parseTags

extractDivTextWithClass :: StringLike a => a -> [Tag a] -> a
extractDivTextWithClass i
    = innerText
    . takeContentTags 1
    . tailSafe
    . dropWhile (not . divOpenWithClass i)

extractDivText :: StringLike a => [a] -> a -> a
extractDivText ids str = strConcat $ extractDivTextWithClass <$> ids <*> [prepTags str]


-----

imgWithClass :: StringLike a => a -> Tag a -> Bool
imgWithClass c = tagOpenAttrLit "img" ("class",c)

extractImgUrl :: Text -> Text -> Maybe Text
extractImgUrl c
    = headMay
    . map (fromAttrib "src")
    . filter (imgWithClass c)
    . prepTags

dropParams :: Text -> Text
dropParams = takeWhile (/= '?')

extractImgUrl_noParams :: Text -> Text -> Maybe Text
extractImgUrl_noParams c = fmap dropParams . extractImgUrl c
