import System.IO
import Data.List
import Data.Char
import Data.List.Split.Internals

rmvDups :: (Ord a) => [a] -> [a]
rmvDups = map head . group . sort

closeEnough :: (Int, Int) -> (Int, Int) -> Bool
closeEnough (x, y) (z, h) =
    sqrt (fromIntegral (z-x) * fromIntegral (z-x) + fromIntegral (h-y) * fromIntegral (h-y)) < 2

table :: String -> Int -> (Int, Int)
table "R" nb = (nb, 0)
table "L" nb = (-nb, 0)
table "U" nb = (0, nb)
table _ nb = (0, -nb)

getVals :: [String] -> (Int, Int)
getVals s = table (head s) (read $ head $ tail s)

-- curr, target, input -> list des coordonnees
updatePos :: (Int, Int) -> (Int, Int) -> [String] -> [(Int, Int)]
updatePos (x, y) (z, h) [] =
    if closeEnough (x, y) (z, h) then
        [(x,y)]
    else
        (x, y) : updatePos (x + signum (z-x), y + signum (h-y)) (z,h) []

updatePos (x, y) (z, h) (s : ts) =
    if closeEnough (x, y) (z, h) then
        updatePos (x, y) (z + newZ, h + newH) ts
    else
        (x, y) : updatePos (x + signum (z-x), y + signum (h-y)) (z,h) (s:ts)
    where
        (newZ, newH) = getVals (splitOn " " s)

main = do
        handle <- readFile "input"

        print "First puzzle:"
        print $ length $ rmvDups $ updatePos (0,0) (0,0) (lines handle)
        print "Second puzzle:"
