module Main where

--import Import
import Menu
import CheckData
import System.Console.ANSI
import System.IO
import System.Environment

-- Основные функции

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    args <- getArgs 
    data_ <- readFile (args !! 3)
    if checkSymbols (lines data_) then if allTheSame (eqData (lines data_)) then chooseOption 0 args data_
                                                                            else do
                                                                                setSGR [SetColor Foreground Vivid Red]
                                                                                putStrLn "ERROR\nРазная длина строк в файле"
                                  else do
                                      setSGR [SetColor Foreground Vivid Red]
                                      putStrLn "ERROR\nНекорректный символ в читаемом файле"
