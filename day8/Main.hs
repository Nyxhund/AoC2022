import System.IO
import Data.List
import Data.List.Split
import Data.Char
import Data.List.Split.Internals

fromTree :: Int -> [Int] -> Bool
fromTree tree [] = True
fromTree tree (t : tail) =
    if t >= tree then
        False
    else
        fromTree tree tail

myTail :: [a] -> [a]
myTail [] = []
myTail ta = tail ta

myHead :: [[a]] -> [a]
myHead [] = []
myHead t = head t

mapFromTree :: Int -> [[Int]] -> Bool
mapFromTree tree [] = False
mapFromTree tree (l : tl) = fromTree tree l || mapFromTree tree tl

fromCol :: [Int] -> [Int] -> [[Int]] -> [[Int]] -> Int
fromCol [] _ _ _ = 0
fromCol (t : next) prev top down =
    if mapFromTree t (prev : next : myHead top : myHead down : []) then
        1 + fromCol next (t : prev) (myTail top) (myTail down)
    else
        fromCol next (t : prev) (myTail top) (myTail down)

fromMat :: [[Int]] -> [[Int]] -> Int
fromMat [] _ = 0
fromMat (line : tl) prev =
    (fromCol line [] (transpose prev) (transpose tl)) + fromMat tl (line : prev)

------------------------------------------

fromTree2 :: Int -> [Int] -> Int
fromTree2 tree [] = 0
fromTree2 tree (t : tail) =
    if t >= tree then
        1
    else
        1 + fromTree2 tree tail

mapFromTree2 :: Int -> [[Int]] -> Int
mapFromTree2 tree [] = 1
mapFromTree2 tree (l : tl) = fromTree2 tree l * mapFromTree2 tree tl


fromCol2 :: [Int] -> [Int] -> [[Int]] -> [[Int]] -> [Int]
fromCol2 [] _ _ _ = []
fromCol2 (t : next) prev top down =
    mapFromTree2 t (prev : next : myHead top : myHead down : []) : fromCol2 next (t : prev) (myTail top) (myTail down)

fromMat2 :: [[Int]] -> [[Int]] -> [[Int]]
fromMat2 [] _ = []
fromMat2 (line : tl) prev =
    (fromCol2 line [] (transpose prev) (transpose tl)) : fromMat2 tl (line : prev)

mapMax :: [[Int]] -> Int -> Int
mapMax [] ret = ret
mapMax (l : tl) ret = mapMax tl (maximum [ret, maximum l])

main = do
        handle <- readFile "input"
        let content = map (map digitToInt) (lines handle)
        print "First puzzle:"
        print $ fromMat content []
        print "Second puzzle:"
        print $ mapMax (fromMat2 content []) 0
