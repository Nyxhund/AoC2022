import System.IO
import Data.List
import Data.Char
import Data.List.Split.Internals

getData :: String -> Int
getData "noop" = 0
getData s = read $ last $ splitOn " " s

buildRec :: [String] -> (Int, Int) -> Int -> [Int]
buildRec [] (a,b) reg = []
buildRec (s:ts) (curr, next) reg =
    if curr /= 0 then
        reg : reg + curr : buildRec ts (next, getData s) (reg + curr)
    else
        reg : buildRec ts (next, getData s) (reg + curr)

buildCrt :: Int -> [Int] -> String
buildCrt _ [] = []
buildCrt i (x:tx) =
    (if abs ((mod i 40) - x) < 2 then 'X' else '.') : buildCrt (i+1) tx

--printCrt :: String -> String
--printCrt [] = []
--printCrt s = print (take 40 s); printCrt (drop 40 ts)

main = do
        handle <- readFile "input"

        print "First puzzle:"
        let record = buildRec (lines handle) (0,0) 1
        --print $ record
        print $ (record!!20 * 20 + record!!60 * 60 + record!!100 * 100 + record!!140 * 140 + record!!180 * 180 + record!!220 * 220)
        print "Second puzzle:"

        let record1 = buildCrt 0 (tail record)
        print $ (take 40 record1)
        let record2 = drop 40 record1
        print $ (take 40 record2)

        let record3 = drop 40 record2
        print $ (take 40 record3)
        let record4 = drop 40 record3
        print $ (take 40 record4)
        let record5 = drop 40 record4
        print $ (take 40 record5)
        let record6 = drop 40 record5
        print $ (take 40 record6)
