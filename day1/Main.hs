import System.IO  
import Data.List

parseElves :: [String] -> [Int] -> [[Int]]
parseElves [] ret = [ret]
parseElves (s:ts) ret = if s == "" then
                  ret : parseElves ts []
                else
                  parseElves ts (read s : ret)

main = do  
        handle <- readFile "input.txt"
        let content = lines handle

        let elves = parseElves content []
        let list = sort(reduceToEnergy elves)
        let top3 = take 3 (reverse list)
        
        print (show (sum top3))

reduceToEnergy :: [[Int]] -> [Int]
reduceToEnergy = map sum

maxInList :: [Int] -> Int
maxInList [] = 0
maxInList l = foldr max 0 l
