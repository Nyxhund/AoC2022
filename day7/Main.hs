import System.IO
import Data.List
import Data.Char
import Data.List.Split.Internals

getVal :: String -> Int
getVal s = if s == "dir" then
                -1
           else
                read s

handleDir :: [String] -> [(String, Int)] -> ([String], [(String, Int)])
handleDir [] accu = ([], accu)
handleDir (s : ts) accu =
    if first == "$" then
        (s : ts, accu)
    else
        handleDir ts (accu ++ [(l!!1, getVal first)])
    where
        l = split (dropDelims $ oneOf " ") s
        first = head l


handleTree :: [String] -> [[(String, Int)]] -> [[(String, Int)]]
handleTree [] accu = accu
handleTree (s : ts) accu =
    if dir == ".." then
        handleTree ts accu
    else
        let (newS, dirVal) = handleDir (drop 1 ts) [] in
        handleTree newS (((dir, 0) : dirVal) : accu)
    where
        l = split (dropDelims $ oneOf " ") s
        dir = l!!2

getReference :: [(String, Int)] -> String -> Int
getReference [] ref = 0
getReference ((s, val) : ts) ref =
    if s == ref then
        val
    else
        getReference ts ref

lookUp :: String -> [(String, Int)] -> Int
lookUp look [] = 0
lookUp look ((s, val) : ts) =
    if look == s then
        val
    else
        lookUp look ts

addUp :: [(String, Int)] -> [(String, Int)] -> Int
addUp [] ref = 0
addUp ((s, val) : ts) ref =
    if val == -1 then
        lookUp s ref + addUp ts ref
    else
        val + addUp ts ref

reduceVals :: [[(String, Int)]] -> [(String, Int)]-> [(String, Int)]
reduceVals [] ref = []
reduceVals (((s, val) : td) : ts) ref = (s, sum) : reduceVals ts ((s, sum) : ref)
    where
        sum = addUp td ref

sumVals :: [(String, Int)] -> Int -> Int
sumVals [] limit = 0
sumVals ((dir, val) : ts) limit =
    if val <= limit then
        val + sumVals ts limit
    else
        sumVals ts limit

sumValsAll :: [(String, Int)] -> Int
sumValsAll [] = 0
sumValsAll ((dir, val) : ts) =
    if dir == "/" then
        val
    else
        sumValsAll ts

minToSup :: [(String, Int)] -> Int -> Int -> Int
minToSup [] limit size = size
minToSup ((s, val) : ts) limit size =
    if (&&) (val > limit) (val < size) then
        minToSup ts limit val
    else
        minToSup ts limit size

main = do
        handle <- readFile "input"

        --print $ handleTree (lines handle) []
        print "First puzzle:"
        print $ sumVals (reduceVals (handleTree (lines handle) []) []) 100000
        print "Second puzzle:"
        let table = reduceVals (handleTree (lines handle) []) []
        print $ table
        print $ sumValsAll table
        print $ (70000000 - sumValsAll table)
        print $ 30000000 - (70000000 - sumValsAll table)
        print $ minToSup table (30000000 - (70000000 - sumValsAll table)) maxBound
