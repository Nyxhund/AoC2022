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
updatePos :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
updatePos (x, y) (z, h) =
    if closeEnough (x, y) (z, h) then
        [(x,y)]
    else
        (x, y) : updatePos (x + signum (z-x), y + signum (h-y)) (z,h)

mapUpdatePos :: [(Int, Int)] -> [[(Int, Int)]]
mapUpdatePos [] = []
mapUpdatePos [k] = [[k]]
mapUpdatePos [prevKnot, lastKnot] = [updatePos lastKnot prevKnot]
mapUpdatePos (prevKnot : currKnot : tl) =
    knotLog : mapUpdatePos (updated : tl)
    where
        knotLog = updatePos currKnot prevKnot
        updated = head $ reverse knotLog

getKnotPos :: [[(Int, Int)]] -> [(Int, Int)]
getKnotPos [] = []
getKnotPos (knot : tknot) = (head $ reverse knot) : getKnotPos tknot

whereAreWe :: [String] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
whereAreWe [] log _ = log
whereAreWe (s:ts) log knots =
    whereAreWe ts (log ++ trace) newKnotsPos
    where
        (newX, newY) = getVals (splitOn " " s)
        (x,y) = head knots
        updateMatrice = [(x + newX, y + newY)] : mapUpdatePos ((x + newX, y + newY) : (tail knots))
        trace = head $ reverse updateMatrice
        newKnotsPos = getKnotPos updateMatrice

main = do
        handle <- readFile "input"

        print "First puzzle:"
        print $ length $ rmvDups $ whereAreWe (lines handle) [] (take 2 (repeat (0,0)))
        print "Second puzzle:"
        print $ length $ rmvDups $ whereAreWe (lines handle) [] (take 10 (repeat (0,0)))
