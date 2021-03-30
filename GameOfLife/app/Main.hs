module Main where

import Lib
import Data.Maybe
import Control.Concurrent
--import System.Console.ANSI


-- Основные типы 

type Epoch = Integer
type Point = (Int, Int)
type Alive = Bool
type Cell  = (Point, Alive)
type Board  = [Cell]


-- Основные функции

directions :: [Point] 
directions = [(-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0)]

live_or_die :: Char -> Bool
live_or_die c 
    | c `== '*'  = True
    | c == '.'  = False

make_row :: String -> Int -> [Cell]
make_row row j_pos = [((i, j_pos), live_or_die (row !! i)) | i <- [0..length row - 1]]

read_data :: [String] -> Board
read_data data_ = concat [make_row (data_ !! j) j | j <- [0..length data_ - 1]]

get_cell :: Point -> Board -> Maybe Cell
get_cell pos [] = Nothing
get_cell pos (((i, j), alive) : xs)
    | pos == (i, j) = Just ((i, j), alive)
    | otherwise = get_cell pos xs 

is_alive :: Board -> Point -> Bool
is_alive board pos
    | isNothing(get_cell pos board) = False
    | snd(fromJust(get_cell pos board)) = True
    | otherwise = False

alive_neighbours :: Board -> Cell -> [Point] -> Integer -> Integer
alive_neighbours board ((i, j), alive) directions cnt
    | null directions = cnt
    | is_alive board (i + fst(head directions), j + snd(head directions)) = alive_neighbours board ((i, j), alive) (tail directions) (cnt + 1)
    | otherwise = alive_neighbours board ((i, j), alive) (tail directions) cnt

next_cell_state :: Integer -> Bool -> Bool
next_cell_state cnt_live_neighbours alive 
    | cnt_live_neighbours == 3 && alive = True
    | cnt_live_neighbours == 3 && not alive = True
    | cnt_live_neighbours == 2 && alive = True
    | otherwise = False 

make_cell :: Cell -> Board -> Cell
make_cell cell board = (fst cell, next_cell_state (alive_neighbours board cell directions 0) (snd cell))

next_state :: Board -> Board
next_state board = map (`make_cell` board) board

representation :: Alive -> String
representation alive 
    | alive = "*"
    | otherwise = "."

put_cell :: Cell -> IO ()
put_cell cell 
    | fst (fst cell) == 0 = putStr ("\n" ++ representation (snd cell))
    | otherwise = putStr (representation (snd cell))

clear_screen :: IO()
clear_screen = putStr "\ESC[2J"

draw_board :: Board -> IO ()
draw_board board = do
    sequence_ [put_cell cell | cell <- board]
    clear_screen
    threadDelay 100000
    draw_board (next_state board) 

main :: IO ()
main = do data_ <- readFile "./map.txt"
          draw_board (read_data (lines data_))



