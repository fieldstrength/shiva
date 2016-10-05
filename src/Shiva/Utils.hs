module Shiva.Utils (

  nothingMsg,
  safeHead,
  safeLast,
  zipWithDefault,
  separate,

) where


nothingMsg :: String -> Maybe a -> Either String a
nothingMsg s Nothing  = Left s
nothingMsg _ (Just x) = Right x

----


safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _     = Nothing

safeLast :: [a] -> Maybe a
safeLast = safeHead . reverse

----


zipWithDefault :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
zipWithDefault _ _ [] [] = []
zipWithDefault f d xs [] = zipWith f xs (repeat d)
zipWithDefault f d [] ys = zipWith f (repeat d) ys
zipWithDefault f d (x:xs) (y:ys) = f x y : zipWithDefault f d xs ys

----

-- return elements of an array before first occurence of special element
takeUntil :: Eq a => a -> [a] -> [a]
takeUntil _ []     = []
takeUntil y (x:xs) = if x == y then [] else x : takeUntil y xs

-- return elements of an array after first occurence of special element
takeAfter :: Eq a => a -> [a] -> [a]
takeAfter _ []     = []
takeAfter y (x:xs) = if x == y then xs else takeAfter y xs

-- return a list of lists corresponding to sublists separated by the special element
separate :: Eq a => a -> [a] -> [[a]]
separate _ [] = []
separate x l | null (takeAfter x l) = [takeUntil x l]
             | otherwise            = takeUntil x l : separate x (takeAfter x l)
