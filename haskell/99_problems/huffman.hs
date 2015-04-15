import Data.Function (on)
import Data.List
import qualified Data.Map.Strict as Map


-- Q 50: Huffman codes.
huffman :: [(a, Int)] -> [(a, String)]
huffman xs =
    let n               = length xs
        reverseFreqList = reverse (map fst $ sortBy (compare `on` snd) xs)
        codes           = [ (replicate i '1') ++ "0" | i <- [0..(n-1)] ] ++ [replicate n '1']
    in  zip reverseFreqList codes

type Encoding a = [(a, String)]
type Code = String
huffEncode :: (Ord a) => [a] -> (Code, Encoding a)
huffEncode xs =
    let encoding  = huffman (freqs xs)
        encoder   = Map.fromList (encoding)
        code      = concatMap (encoder Map.!) xs
    in (code, encoding)

freqs :: (Ord a) => [a] -> [(a, Int)]
freqs xs = map (\ ys -> (head ys, length ys)) (group . sort $ xs)
