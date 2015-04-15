import System.Environment (getArgs)

main :: IO ()
main = do
  putStr "Enter a number: "
  a <- getLine
  putStr "Enter another: "
  b <- getLine
  putStrLn $ "Sum = " ++ (sumStrings a b)


sumStrings :: String -> String -> String
sumStrings a b = show(aInt + bInt)
  where aInt = read(a) :: Int
        bInt = read(b) :: Int
