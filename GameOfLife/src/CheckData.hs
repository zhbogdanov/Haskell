module CheckData(
    allTheSame,
    eqData,
    checkSymbols,
    isEqBoard
) where

import Types

isCurrentSymbol :: String -> Bool
isCurrentSymbol (x:xs) = foldr (\ x -> (&&) ((x == '*') || (x == '.'))) True xs

checkSymbols :: [String] -> Bool
checkSymbols (x:xs) = foldr ((&&) . isCurrentSymbol) True xs

allTheSame :: [Int] -> Bool
allTheSame xs = all (== head xs) (tail xs)

eqData :: [String] -> [Int]
eqData data_ = [length (data_ !! i) | i <- [0..length data_ - 1]]

-- равны ли состония клеток
isEqCell :: Cell -> Cell -> Bool
isEqCell cell1 cell2 = snd cell1 == snd cell2

-- равны ли доски
isEqBoard :: Board -> Board -> Bool -> Bool
isEqBoard board1 board2 f
    | not f = False
    | null board1 = True
    | otherwise = isEqBoard (tail board1) (tail board2) (isEqCell (head board1) (head board2))