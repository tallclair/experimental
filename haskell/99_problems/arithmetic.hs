-- Q 31: Determine whether a given integer number is prime.
isPrime :: (Integral a) => a -> Bool
isPrime x = helper x (floor $ sqrt (fromIntegral x))
    where helper _ 1 = True
          helper x y = if (y * (x `div` y) == x) then False else helper x (y - 1)

factor :: (Integral a) => a -> (a, a)
factor x = helper x (floor $ sqrt (fromIntegral x))
    where helper x 1 = (x, 1)
          helper x y = if (y * (x `div` y) == x) then ((x `div` y), y) else helper x (y - 1)

-- Q 32: Determine the greatest common divisor of two positive integer numbers.
myGCD :: (Integral a) => a -> a -> a
myGCD x y
    | x `mod` y == 0 = y
    | otherwise      = myGCD y (x `mod` y)
