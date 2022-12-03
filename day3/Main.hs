import System.IO  
import Data.List
import Data.Char

getValue :: Char -> Int
getValue c = if ord c < ord 'a' then
                ord c - ord 'A' + 27
             else
                ord c - ord 'a' + 1

splitMatches :: [String] -> [(String, String)]
splitMatches [] = []
splitMatches (s : ts) = (splitAt (div (length s) 2) s) : splitMatches ts

commonElement :: [(String, String)] -> [Int]
commonElement [] = []
commonElement ((a, b) : ts) = (getValue (head (intersect a b))) : commonElement ts

splitBy3 :: [String] -> [[String]]
splitBy3 [] = []
splitBy3 s = (take 3 s) : (splitBy3 $ drop 3 s)

intersectOfIntersect :: [String] -> Char
intersectOfIntersect l = head(intersect (l!!0) (intersect (l!!1) (l!!2)))

commonElement2 :: [[String]] -> [Int]
commonElement2 [] = []
commonElement2 (s : ts) = (getValue (intersectOfIntersect s)) : commonElement2 ts

main = do  
        handle <- readFile "input.txt"
        let content1 = splitMatches $ lines handle

        print $ show $ "Solution 1 is: "
        print $ sum $ commonElement content1

        let content2 = splitBy3 $ lines handle
        print $ sum $ commonElement2 content2