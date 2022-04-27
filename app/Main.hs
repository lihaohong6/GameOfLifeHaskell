module Main where
import Control.Concurrent
import Board (Board)
main :: IO ()
main = do 
 putStrLn "Please input your file name: "
 fileName <- getLine
 f <- readFile fileName
 putStrLn (show (read f :: Board))
 threadDelay 1000000
