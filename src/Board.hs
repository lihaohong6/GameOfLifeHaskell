module Board where

import Data.Char (chr)


data Row
  = Row {len :: Int, cells :: [Bool]}
  deriving Eq
  
instance Show Row where
  show (Row _ c) = map (\x -> chr (48 + if x then 1 else 0)) c

stringToList :: String -> [Bool]
stringToList "" = []
stringToList (x:xs) = (x == '1') : stringToList xs

instance Read Row where
  readsPrec _ s = let r = stringToList s
    in [(Row (length r) (stringToList s), "")]

makeRow :: [Bool] -> Row
makeRow list = Row (length list) list

-- num rows, num columns, list of rows
data Board = Board {rowNum :: Int, columnNum ::Int, boardData :: [Row]}
  deriving (Eq, Show)

instance Read Board where
  readsPrec _ s =
    let b = map read (lines s) :: [Row]
     in [(Board (length b) (len (head b)) b, "")]

-- zip everything and lazily evaluate them

