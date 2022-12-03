import System.IO  
import Data.List
import Data.Char

splitMatches :: [String] -> [(Char, Char)]
splitMatches = map (\s -> (s!!0, s!!2))

getValuePlay :: Char -> Char -> Int
getValuePlay c base = ord c - ord base + 1

getValueOpponentPlay :: Char -> Int
getValueOpponentPlay c = ((-(ord c - ord 'C')) + 1) `mod` 3

getVictory1 :: (Char, Char) -> Int
getVictory1 (a, b) = ((getValuePlay b 'X' + getValueOpponentPlay a) `mod` 3) * 3

getPoint1 :: (Char, Char) -> Int
getPoint1 (a, b) = getValuePlay b 'X' + getVictory1 (a, b)

getVictory2 :: Char -> Int
getVictory2 c = (ord c - ord 'X') * 3

getValuePlay2 :: Int -> Int -> Int
getValuePlay2 play offset = ((play + offset) `mod` 3) + 1

getPoint2 :: (Char, Char) -> Int
getPoint2 (a, b) = getVictory2 b + getValuePlay2 (getValuePlay a 'A') (getValuePlay b 'X')

main = do  
        handle <- readFile "input.txt"
        let content = splitMatches $ lines handle
        print "First stategy guide gives :"
        print $ show $ sum $ map getPoint1 content

        print ""
        print "Second Strategy guide gives :"
        print $ show $ sum $ map getPoint2 content