module Board where

import Data.Char (chr)


data Row
  = Row {len :: Int, cells :: [Bool]}
  deriving Eq

-- '●' is living cell, '◯' is dead cell
instance Show Row where
  -- (\x -> if x then '■' else '□')
  show (Row _ c) = map (\x -> if x then '●' else '◯') c

stringToList :: String -> [Bool]
stringToList "" = []
stringToList (x:xs) = (x == '1') : stringToList xs

instance Read Row where
  readsPrec _ s = let r = stringToList s
    in [(Row (length r) (stringToList s), "")]

makeRow :: [Bool] -> Row
makeRow list = Row (length list) list

data Board = Board {rowNum :: Int, columnNum ::Int, boardData :: [Row]}
  deriving (Eq)

instance Show Board where
  show (Board _ _ b) = unlines (map show b)

instance Read Board where
  readsPrec _ s =
    let b = map read (lines s) :: [Row]
     in [(Board (length b) (len (head b)) b, "")]


