module Board where

import Data.Char (chr)

-- len is the length of the row, and cells is the list of Bools that
-- represent the cells on the board. 
data Row
  = Row {len :: Int, cells :: [Bool]}
  deriving Eq
<<<<<<< HEAD
  
-- If the number is 1 then out put '●', otherwise output '◯'.
-- '●' is living cell, '◯' is dead cell
instance Show Row where
  -- (\x -> if x then '■' else '□')
  show (Row _ c) = map (\x -> if x then '●' else '◯') c

-- Translate the string of  0 and 1s to a list of string
stringToList :: String -> [Bool]
stringToList "" = []
stringToList (x:xs) = (x == '1') : stringToList xs

-- read the input information about rows
-- 
instance Read Row where
  readsPrec _ s = let r = stringToList s
    in [(Row (length r) (stringToList s), "")]

makeRow :: [Bool] -> Row
makeRow list = Row (length list) list

-- the data Board consists of its row number, col number and a list of 
-- Rows that represents the cells on the board
data Board = Board {rowNum :: Int, columnNum ::Int, boardData :: [Row]}
  deriving (Eq)

-- it maps the list of rows to a string
instance Show Board where
  show (Board _ _ b) = unlines (map show b)

-- read the input information about board
-- b is the list of rows and head b is the first row of the board
-- therefore, len(b) - num of rows, and len(head b) is the num of cols
instance Read Board where
  readsPrec _ s =
    let b = map read (lines s) :: [Row]
     in [(Board (length b) (len (head b)) b, "")]


