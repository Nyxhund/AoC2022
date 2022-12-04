import System.IO  
import Data.List
import Data.Char
import Data.List.Split.Internals

myDiv :: Int -> Int -> Int
myDiv a b = if a - b == 0 then
                0
            else
                (a - b) `div` abs(a-b)

getValue :: [[[Int]]] -> Int
getValue [] = 0
getValue (l : tl) =
    let a = head(head l)
        b = (head l)!!1
        c = head(l!!1)
        d = (l!!1)!!1
    in
    if (myDiv a c == 0 && myDiv b d == 0) || myDiv a c /= myDiv b d then
        1 + getValue tl
    else
        getValue tl

getValue2 :: [[[Int]]] -> Int
getValue2 [] = 0
getValue2 (l : tl) =
    let a = head(head l)
        b = (head l)!!1
        c = head(l!!1)
        d = (l!!1)!!1
    in
    if (a >= c && a <= d) || (b >= c && b <= d) || (myDiv a c == 0 && myDiv b d == 0) || myDiv a c /= myDiv b d then
        1 + getValue2 tl
    else
        getValue2 tl

splitMatches :: [String] -> [[[Int]]]
splitMatches [] = []
splitMatches (s : ts) = (Data.List.map (Data.List.map read) (Data.List.map (split (dropDelims $ oneOf "-")) (split (dropDelims $ oneOf ",") s))) : splitMatches ts


main = do
        handle <- readFile "input"
        let content1 = splitMatches $ lines handle
        print $ show $ getValue content1
        print $ show $ getValue2 content1
