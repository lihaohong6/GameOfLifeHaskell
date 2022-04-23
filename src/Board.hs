module Board where

import Data.Char (chr)


data Row
  = Row {len :: Int, alive :: Bool, cells :: [Bool]}
  deriving Eq
  
instance Show Row where
  show (Row _ _ c) = map (\x -> chr (48 + if x then 1 else 0)) c

makeRow :: [Bool] -> Row
makeRow list = Row (length list) (or list) list

emptyRow :: Row
emptyRow = Row 0 False (repeat False)

-- num rows, num columns, list of rows
data Board = Board Int Int [Row]
  deriving (Eq, Show)

-- zip everything and lazily evaluate them

