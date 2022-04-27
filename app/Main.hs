module Main where
import Control.Concurrent
import Board (Board)
import System.IO (stdout, hFlush)
import Simulation

printNext :: [Board] -> IO ()
printNext (b:xs) = print b >> hFlush stdout >> threadDelay 300000 >> printNext xs

main :: IO ()
main = do 
 putStrLn "Please input your file name: "
 hFlush stdout
 fileName <- getLine
 f <- readFile fileName
 printNext (iterate next (read f))
