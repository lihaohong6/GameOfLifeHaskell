module Main where

import Board (Board)
import Control.Concurrent
import Control.Exception.Base (try)
import Simulation
import System.IO (hFlush, stdout)

printNext :: [Board] -> IO ()
printNext (b : xs) = print b >> hFlush stdout >> threadDelay 300000 >> printNext xs

getFileString :: IO String
getFileString = do
  putStrLn "Please input your file path: "
  hFlush stdout
  fn <- getLine
  strOrExc <- try $ readFile fn :: IO (Either IOError String)
  case strOrExc of
    Left err ->
      do
        print err
        getFileString
    Right contents ->
      return contents

main :: IO ()
main = do
  s <- getFileString
  printNext (iterate next (read s))
