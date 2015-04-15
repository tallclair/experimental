
collatz :: (Integral a) => a -> [a]
collatz 1 = []
collatz x = y : collatz y
    where y | odd x = 3 * x + 1
            | otherwise = x `div` 2


argmax :: (Ord a) => [a] -> Int
argmax x = fst (argmaxHelper x)

argmaxHelper :: (Ord a) => [a] -> (Int, a)
argmaxHelper [] = error "empty list"
argmaxHelper (x:[]) = (1, x)
argmaxHelper (x:xs)
    | x > max = (1, x)
    | otherwise = (arg + 1, max)
    where (arg, max) = argmaxHelper xs
