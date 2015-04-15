-- file: ch04/InteractWith.hs
-- Save this in a source file, e.g. Interact.hs

import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = transposeLines

transposeLines :: String -> String
transposeLines = unlines . transpose . lines

transpose :: [String] -> [String]
transpose [] = []
transpose xs
  | all null xs = []
  | otherwise = (map paddedHead xs) : transpose (map paddedTail xs)

paddedHead :: String -> Char
paddedHead "" = ' '
paddedHead s  = head s

paddedTail :: String -> String
paddedTail "" = ""
paddedTail s  = tail s
