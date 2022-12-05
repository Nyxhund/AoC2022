import System.IO
import Data.List
import Data.Char
import Data.List.Split.Internals

addItem :: String -> Int -> [Char] -> [Char]
addItem s index col = if s!!(index * 4 + 1) /= ' ' then
                        col ++ [s!!(index * 4 + 1)]
                    else
                        col

mapIndexIncrease :: String -> [[Char]] -> Int -> [[Char]]
mapIndexIncrease s [] i = []
mapIndexIncrease s (c : cols) i = (addItem s i c) : mapIndexIncrease s cols (i+1)

parseColumns :: [String] -> [[Char]] -> ([[Char]], [String])
parseColumns [] cols = (cols, [])
parseColumns ("" : ts) cols = (cols, ts)
parseColumns (s : ts) cols = parseColumns ts (mapIndexIncrease s cols 0)

initCols :: Int -> [[a]]
initCols 0 = []
initCols n = [] : initCols (n-1)

parseCommands :: [String] -> [[Int]]
parseCommands [] = []
parseCommands (s : ts) = (map read (c!!1 : c!!3 : [c!!5])) : parseCommands ts
                where
                c = split (dropDelims $ oneOf " ") s

----------------------------------------------------------------------------

takeItems :: Int -> Int -> [[Char]] -> ([Char], [[Char]])
--                    add a reverse here for part 1, sorry
takeItems nb i cols = (take nb (cols!!i), (take i cols) ++ [(drop nb (cols!!i))] ++ (drop (i + 1) cols))

moveItems :: [Int] -> [[Char]] -> [[Char]]
moveItems [i, a, b] cols = (take (b-1) newCols) ++ [(insert ++ (newCols!!(b-1)))] ++ (drop (b) newCols)
                            where
                            (insert, newCols) = takeItems i (a-1) cols

updateCol :: [[Int]] -> [[Char]] -> [[Char]]
updateCol [] cols = cols
updateCol (command : tailCommand) cols = updateCol tailCommand (moveItems command cols)

getAnswer :: [[Char]] -> String
getAnswer = map head

main = do
        handle <- readFile "input"
        let content = lines handle
        let (cols, rest) = parseColumns content (initCols 9)
        print $ getAnswer $ updateCol (parseCommands rest) cols
