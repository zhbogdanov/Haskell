module Menu(
    chooseOption
) where 

import GameRules
import Types
import System.Console.ANSI
import System.IO
import System.Directory
import Data.List

liveSymbols = ["*", "[●]", "!", "?", "?!"]
deadSymbols = [".", "[.]", "1", "2", "3"]
menu_ = ["Показать файлы с результатами", "Играть"]
currentdir = "/Users/polzovatel/Documents/AL/Haskell/haskell-2021/"

getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

stringToInt :: String -> Int
stringToInt = read

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot pred = filter $ not . pred

showFile :: [String] -> String -> [String] -> String -> IO()
showFile args data_ dir file = do
    clearScreen
    file_ <- readFile (currentdir ++ "LeaderBoard/" ++ file)
    putStrLn file_
    setSGR [SetColor Foreground Vivid Green]
    putStrLn "\n\nДля выхода нажмите q"
    setSGR [Reset]
    ss <- getKey
    case ss of 
        "q" -> showFiles 0 args data_ dir 
        _ -> showFile args data_ dir file

showFiles :: Int -> [String] -> String -> [String] -> IO()
showFiles i args data_ dir = do
    clearScreen
    setSGR [SetColor Foreground Vivid Green]
    putStrLn "Выберете файл. Для подтверждения нажмите на пробел. Для выхода нажмите q\n\n"
    setSGR [Reset]
    putMenu i 0 "\n" dir
    ss <- getKey
    case ss of
        " " -> showFile args data_ dir (dir !! i)
        "q" -> chooseOption 0 args data_
        "\ESC[A" -> if i == 0 then showFiles (length dir - 1)  args data_ dir else showFiles (i - 1) args data_ dir
        "\ESC[B" -> if i == length dir - 1 then showFiles 0 args data_ dir else showFiles (i + 1) args data_ dir
        _ -> showFiles i args data_ dir

chooseOption :: Int -> [String] -> String -> IO()
chooseOption i args data_ = do
    clearScreen
    setSGR [SetColor Foreground Vivid Green]
    putStrLn "Для выбора нажмите пробел. Для выхода нажмите q\n\n"
    setSGR [Reset]
    putMenu i 0 "\n" menu_
    ss <- getKey
    case ss of
        " " -> do
            case i of 
                0 -> do
                    all <- getDirectoryContents (currentdir ++ "LeaderBoard")
                    let dir = filterNot (isPrefixOf ".") all
                    showFiles i args data_ dir
                1 -> do
                    loop 0 "живых" (head liveSymbols) (head deadSymbols) (stringToInt (head args)) (readData (lines data_)) (args !! 1) (args !! 2) 0 liveSymbols (length liveSymbols)
        "q" -> do
                clearScreen 
                return()
        "\ESC[A" -> if i == 0 then chooseOption (length menu_ - 1) args data_ else chooseOption (i - 1) args data_
        "\ESC[B" -> if i == length menu_ - 1 then chooseOption 0 args data_ else chooseOption (i + 1) args data_ 
        _ -> chooseOption i args data_

putMenu :: Int -> Int -> String -> [String] -> IO()
putMenu i j sep (x:xs) = do
    if i == j then do 
                    setSGR [SetColor Foreground Vivid Red]
                    putStr (x ++ sep)
                    setSGR [Reset]
              else putStr (x ++ sep)
    putMenu i (j+1) sep xs
putMenu i j _ _ = putStrLn ""

loop :: Int -> String -> String -> String -> Int -> Board -> String -> String -> Int -> [String] -> Int -> IO ()
loop i mode live dead delay cur_board user file epochs menu len = do
    clearScreen
    setSGR [SetColor Foreground Vivid Green]
    putStrLn ("Выберете символ для " ++ mode ++ " клеток. Для подтверждения нажмите на пробел\n\n")
    setSGR [Reset]
    putMenu i 0 " " menu
    ss <- getKey
    case ss of
       " " -> do
           case mode of 
               "живых" -> loop 0 "мертвых" (menu !! i) dead  delay cur_board user file epochs deadSymbols (length deadSymbols)
               "мертвых" -> drawBoard1 live (menu !! i) delay cur_board user file epochs
       "\ESC[C" -> if i == len - 1 then loop 0 mode live dead  delay cur_board user file epochs menu len else loop (i+1) mode live dead  delay cur_board user file epochs menu len
       "\ESC[D" -> if i == 0 then loop (len - 1)  mode live dead  delay cur_board user file epochs menu len else loop (i-1) mode live dead  delay cur_board user file epochs menu len
       _ -> loop i mode live dead delay cur_board user file epochs menu len