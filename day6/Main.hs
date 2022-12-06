import System.IO
import Data.List
import Data.Char
import Data.List.Split.Internals

checkChars :: String -> String -> Bool
checkChars [] pattern = True
checkChars (c : s) pattern = (&&) (length(intersect pattern [c]) == 1) (checkChars s pattern)

countChars :: String -> Int -> Int
countChars [] i = 0
countChars (c : s) i =  if checkChars (take i (c:s)) (take i (c:s)) then
                            i
                        else
                            1 + countChars s i

main = do
        handle <- readFile "input"
        print "First puzzle:"
        print $ countChars handle 4
        print "Second puzzle:"
        print $ countChars handle 14
