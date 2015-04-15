-- file: ch04/ch04.exercises.hs

import Data.Char (digitToInt)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just $ tail xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just $ init xs


splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs
  | null (takeWhile f xs) = [xs]
  | otherwise = (takeWhile f xs) : splitWith f (dropWhile f xs)


asInt_fold :: String -> Int
asInt_fold ('-':s) = (- asInt_fold s)
asInt_fold s = foldl (\ x c -> x * 10 + digitToInt c) 0 s

concat' :: [[a]] -> [a]
concat' = foldr (++) []
