import System.IO
import Data.List
import Data.Char
import Data.List.Split.Internals

calculateDir :: ([String], Int, [Int]) -> ([String], Int, [Int])
calculateDir ([], actual, list) = ([], actual, actual : list)
calculateDir ((s:ts), actual, list) =
    if first == "$" then
        if l!!1 == "ls" then
            calculateDir (ts, actual, list)
        else if l!!2 == ".." then
            (ts, actual, actual : list)
        else
            let (newS, ret, newList) = calculateDir (ts, 0, list) in
            calculateDir (newS, (actual + ret), newList)
    else
        if first == "dir" then
            calculateDir (ts, actual, list)
        else
            calculateDir (ts, actual + read first, list)
    where
        l = split (dropDelims $ oneOf " ") s
        first = head l

main = do
        handle <- readFile "input"

        print "First puzzle:"
        let (_, _, list) = calculateDir (lines handle, 0, [])
        print $ foldl (\accu i -> accu + if i >= 100000 then 0 else i) 0 (tail list)
        print "Second puzzle:"
        let minToFree = 30000000 - (70000000 - head list)
        print $ foldl1 (\accu i -> if i >= minToFree && i < accu then i else accu) (tail list)
