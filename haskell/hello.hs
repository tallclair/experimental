doubleMe x = x + x
doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100
                      then x
                      else x*2



triangles = [(a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10], a*a + b*b == c*c]


fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)


factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n - 1)




