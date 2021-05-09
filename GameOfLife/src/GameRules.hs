module GameRules(
    drawBoard1,
    readData
) where

import CheckData
import WorkFile
import Types
import Data.Bifunctor
import System.Console.ANSI
import Control.Concurrent
import Data.Maybe

directions :: [Point] 
directions = [(-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0)]

liveOrDie :: Char -> Bool
liveOrDie c =
  case c of
    '*' -> True
    '.' -> False

makeRow :: String -> Int -> [Cell]
makeRow row j_pos = [((i, j_pos), liveOrDie (row !! i)) | i <- [0..length row - 1]]

readData :: [String] -> Board
readData data_ = concat [makeRow (data_ !! j) j | j <- [0..length data_ - 1]]

getCell :: Point -> Board -> Maybe Cell
getCell pos [] = Nothing
getCell pos (((i, j), alive) : xs) = if pos == (i, j) then Just ((i, j), alive) else getCell pos xs

isAlive :: Board -> Point -> Bool
isAlive board pos
    | isNothing(getCell pos board) = False
    | snd(fromJust(getCell pos board)) = True
    | otherwise = False

aliveNeighbours :: Board -> Cell -> [Point] -> Integer -> Integer
aliveNeighbours board ((i, j), alive) directions cnt
    | null directions = cnt
    | isAlive board (Data.Bifunctor.bimap (i +) (j +) (head directions)) = aliveNeighbours board ((i, j), alive) (tail directions) (cnt + 1)
    | otherwise = aliveNeighbours board ((i, j), alive) (tail directions) cnt

nextCellState :: Integer -> Bool -> Bool
nextCellState cnt_live_neighbours alive 
    | cnt_live_neighbours == 3 && alive = True
    | cnt_live_neighbours == 3 && not alive = True
    | cnt_live_neighbours == 2 && alive = True
    | otherwise = False 

makeCell :: Cell -> Board -> Cell
makeCell cell board = second (nextCellState (aliveNeighbours board cell directions 0)) cell

nextState :: Board -> Board
nextState board = map (`makeCell` board) board

putPell :: String -> String -> Cell -> IO ()
putPell live dead cell 
    | fst (fst cell) == 0 = if representation live dead (snd cell) == live then
                                (do setSGR [SetColor Foreground Vivid Green]
                                    putStr ("\n" ++ representation live dead (snd cell)))
                            else
                                (do setSGR [SetColor Foreground Vivid Red]
                                    putStr ("\n" ++ representation live dead (snd cell)))
    | otherwise = if representation live dead (snd cell) == live then
                      (do setSGR [SetColor Foreground Vivid Green]
                          putStr (representation live dead (snd cell)))
                  else
                      (do setSGR [SetColor Foreground Vivid Red]
                          putStr (representation live dead (snd cell)))

-- исходная функция для вывода доски
drawBoard1 :: String -> String -> Int -> Board -> String -> String -> Int -> IO ()
drawBoard1 live dead delay cur_board user file epochs = do
    clearScreen
    sequence_ [putPell live dead cell | cell <- cur_board]
    threadDelay delay
    drawBoard live dead delay (nextState cur_board) cur_board user file (epochs + 1)

-- проверяет не надо ли заканчивать
drawBoard :: String -> String -> Int -> Board -> Board -> String -> String -> Int -> IO ()
drawBoard live dead delay cur_board prev_board user file epochs = do
    if isEqBoard cur_board prev_board True
        then writeToFile live dead cur_board user file epochs
        else drawBoard1 live dead delay cur_board user file epochs