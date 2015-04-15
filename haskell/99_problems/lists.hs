import Data.List
import Data.Function (on)

-- Q1: Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Q2:  Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast [_] = error "too few elements"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

-- Q3: Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt x i = x !! (i - 1)

-- Q4: Find the number of elements of a list.
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Q5: Reverse a list.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- Q6: Find out whether a list is a palindrome.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome list = list == reverse list

-- Q7: Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = foldr (\ x acc -> (flatten x) ++ acc) [] x

-- Q8: Eliminate consecutive duplicates of list elements.
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:list@(y:_))
    | x == y = compress list
    | otherwise = x : compress list

compress' :: (Eq a) => [a] -> [a]
compress' = map head . group

-- Q9: Pack consecutive duplicates of list elements into sublists.
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:list@(y:_))
    | x == y = (x : (head $ pack list)) : (tail $ pack list)
    | otherwise = [x] : (pack list)


-- Q10: Run-length encoding of a list.
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\ x -> (length x, head x)) . pack

-- Q11: Modified run-length encoding.
data ListItem a = Single a | Multiple Int a
                  deriving (Show)
encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified x = map itemize $ encode x
    where itemize (1, a) = Single a
          itemize (n, a) = Multiple n a

-- Q12: Decode a run-length encoded list.
decodeModified :: [ListItem a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs) = x:(decodeModified xs)
decodeModified ((Multiple n x):xs) = (replicate n x) ++ (decodeModified xs)

-- Q13: Run-length encoding of a list (direct solution).
encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect [x] = [Single x]
encodeDirect (x:xs) =
    let encoded = encodeDirect xs
    in if match x (head encoded)
       then (increment (head encoded)) : (tail encoded)
       else (Single x) : encoded
    where match val (Single other) = val == other
          match val (Multiple _ other) = val == other
          increment (Single val) = Multiple 2 val
          increment (Multiple n val) = Multiple (n+1) val

-- Q14: Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli = foldr ((++) . replicate 2) []

dupli' :: [a] -> [a]
dupli' [] = []
dupli' (x:xs) = x:x:(dupli xs)

-- Q15: Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli l n = concatMap (replicate n) l

-- Q16: Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery xs n
    | length xs < n  = xs
    | length xs == n = init xs
    | otherwise      = (take (n - 1) xs) ++ (dropEvery (drop n xs) n)

-- Q17: Split a list into two parts; the length of the first part is given.
split :: [a] -> Int -> ([a], [a])
split xs n = ([ x | (x, y) <- zipped, y <= n],
              [ x | (x, y) <- zipped, y > n])
    where zipped = zip xs [1..]

-- Q18: Extract a slice from a list.
slice :: [a] -> Int -> Int -> [a]
slice xs i k
      | i >= k = []
      | otherwise = drop (i-1) $ take k xs

-- Q19: Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate xs n
    | n == 0 = xs
    | n > 0  = (drop n xs) ++ (take n xs)
    | n < 0  = rotate xs (length xs + n)

rotate' :: [a] -> Int -> [a]
rotate' [] _ = []
rotate' l@(x:xs) n
    | n == 0 = xs
    | n < 0  = rotate l (length l + n)
    | n > 0  = rotate (xs ++ [x]) (n - 1)

-- Q20: Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a, [a])
removeAt n l@(x:xs)
         | n <= 0        = error "Index too small"
         | n >= length l = error "Index too large"
         | n == 1        = (x, xs)
         | otherwise     =
             let (item, remainder) = removeAt (n - 1) xs
             in (item, x:remainder)

-- Q21: Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt el xs 1 = el:xs
insertAt _ [] _ = error "Index out of bounds."
insertAt el (x:xs) n = x : (insertAt el xs (n-1))

-- Q22: Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range x y = [x..y]

-- Q23: Extract a given number of randomly selected elements from a list.
-- TODO: Random.

-- Q24: Lotto: Draw N different random numbers from the set 1..M.
-- TODO: Random.

-- Q25: Generate a random permutation of the elements of a list.
-- TODO: Random.

-- Q26: Generate the combinations of K distinct objects chosen from the N elements of a list
combinations :: Int -> [a] -> [[a]]
combinations 1 xs = [ [x] | x <- xs ]
combinations n xs = [ x:combo |
                      (i, x) <- zip [1..] xs, i <= (length xs) - n,
                      combo <- combinations (n-1) (drop i xs) ]

-- Q27: Group the elements of a set into disjoint subsets.
-- TODO

-- Q28: Sorting a list of lists according to length of sublists
--   a)
lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) =
    let shorter = [ y | y <- xs, (length y) <= (length x) ]
        longer  = [ y | y <- xs, (length y) > (length x) ]
    in (lsort shorter) ++ x : (lsort longer)

--   b)
lfsort :: [[a]] -> [[a]]
lfsort [] = []
lfsort xs =
    let sorted = lsort xs
        grouped = groupBy ((==) `on` length) sorted
    in concat (lsort grouped)
