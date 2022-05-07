module Board where

-- len is the length of the row, and cells is the list of Bools that
-- represent the cells on the board. 
data Row
  = Row {len :: Int, cells :: [Bool]}
  deriving Eq

-- If the list element is True then output '●', otherwise output '◯'.
instance Show Row where
  show (Row _ c) = map (\x -> if x then '●' else '◯') c

-- Translate a string of 0s and 1s to a list of Bool
stringToList :: String -> [Bool]
stringToList "" = []
stringToList (x:xs) = (x == '1') : stringToList xs


instance Read Row where
  readsPrec _ s = let r = stringToList s
    in [(Row (length r) (stringToList s), "")]

-- the data Board consists of its row number, col number and a list of 
-- Rows that represents the cells on the board
data Board = Board {rowNum :: Int, columnNum ::Int, boardData :: [Row]}
  deriving (Eq)

-- convert the list of rows to a string
instance Show Board where
  show (Board _ _ b) = unlines (map show b)

-- read the input information about board
-- b is the list of strings and head b is the first row of the board
-- therefore, len(b) is the num of rows, and len (head b) is the num of cols
instance Read Board where
  readsPrec _ s =
    let b = map read (lines s) :: [Row]
     in [(Board (length b) (len (head b)) b, "")]


