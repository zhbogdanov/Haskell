module Types(
    Epoch,
    Point,
    Alive,
    Cell,
    Board
) where

-- Основные типы 

type Epoch = Integer
type Point = (Int, Int)
type Alive = Bool
type Cell  = (Point, Alive)
type Board  = [Cell]