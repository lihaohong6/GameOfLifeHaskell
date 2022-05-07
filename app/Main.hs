module Main where

import Board (Board)
import Control.Concurrent
import Control.Exception.Base (try)
import Simulation
import System.IO (hFlush, stdout)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

printNext :: [Board] -> IO ()
printNext (b : xs) = print b >> hFlush stdout >> threadDelay 300000 >> printNext xs

-- get file path from user
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

-- set up encoding for cell display, get initial configuration, and iterate generations
main :: IO ()
main = do
  setLocaleEncoding utf8
  s <- getFileString
  printNext (iterate next (read s))
