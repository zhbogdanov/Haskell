module WorkFile(
    representation,
    writeToFile
) where

import CheckData
import Types
import GHC.IO.Handle
import System.IO

representation :: String -> String -> Alive -> String
representation live dead alive 
    | alive = live
    | otherwise = dead

-- по одной клетке в файл
putCellToFile :: String -> String -> Handle -> Cell -> IO ()
putCellToFile live dead result cell 
    | fst (fst cell) == 0 = hPutStr result ("\n" ++ representation live dead (snd cell))
    | otherwise =  hPutStr result (representation live dead (snd cell))


-- всю доску в файл
writeToFile :: String -> String -> Board -> String -> String -> Int -> IO ()
writeToFile live dead board user file epochs = do
    result <- openFile ("LeaderBoard/" ++ file) WriteMode
    hPutStrLn result ("Player: " ++ user)
    hPutStr result ("Epochs done: " ++ show epochs)
    sequence_ [putCellToFile live dead result cell | cell <- board]
    hClose result