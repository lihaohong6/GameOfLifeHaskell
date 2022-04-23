module Simulation where

import Board
import Data.Char (chr)

data Triple = Triple Bool Bool Bool
  deriving (Eq)

instance Show Triple where
  show (Triple a b c) = chr (48 + fromEnum a) : chr (48 + fromEnum b) : [chr (48 + fromEnum c)]

emptyTriple :: Triple
emptyTriple = Triple False False False

tupleToTriple :: (Bool, Bool, Bool) -> Triple
tupleToTriple (a, b, c) = Triple a b c

data Cell = Cell Triple Triple Triple
  deriving (Eq)

instance Show Cell where
  show (Cell a b c) = '(' : show a ++ show b ++ show c ++ [')']

tupleToCell :: (Triple, Triple, Triple) -> Cell
tupleToCell (a, b, c) = Cell a b c

zip3ListRecursive :: a -> [a] -> [(a, a, a)]
zip3ListRecursive zero (x : xs : xss : xsss) = (x, xs, xss) : zip3ListRecursive zero (xs : xss : xsss)
zip3ListRecursive zero [x, xs] = [(x, xs, zero)]
zip3ListRecursive _ _ = []

zip3List :: a -> [a] -> [(a, a, a)]
zip3List zero list@(x : xs : _) = (zero, x, xs) : zip3ListRecursive zero list
zip3List _ _ = []

triplesToCells :: [Triple] -> [Cell]
triplesToCells list = map tupleToCell (zip3List emptyTriple list)

reduceCell :: Cell -> Bool
reduceCell (Cell (Triple a b c) (Triple d e f) (Triple g h i)) =
  let s = sum (map fromEnum [a, b, c, d, f, g, h, i]) :: Int
   in (e && (s == 2 || s == 3)) || (not e && s == 3)

appendCell :: Cell -> ([Bool], Int) -> ([Bool], Int)
appendCell cell (lst, num) =
  let c = reduceCell cell
   in (c : lst, num + 1)

reduceRow :: [Cell] -> Row
reduceRow row =
  let (list, num) = foldr appendCell ([], 0) row
   in Row num list

zipRows :: (Row, Row, Row) -> [Triple]
zipRows (Row _ r1, Row _ r2, Row _ r3) = map tupleToTriple (zip3 r1 r2 r3)

rowsToCells :: [Row] -> Int -> [[Cell]]
rowsToCells rows r1 =
  let empty = Row r1 (replicate r1 False)
   in map
        (triplesToCells . zipRows)
        ( zip3
            (empty : rows)
            rows
            (drop 1 rows ++ [empty])
        )

computeRows :: [Row] -> Int -> [Row]
computeRows [] _ = []
computeRows rows l = map reduceRow (rowsToCells rows l)

next :: Board -> Board
next (Board _ columnCount rows) =
  let resultingRows = computeRows rows columnCount
   in Board (length resultingRows) (foldr (max . len) 0 resultingRows) resultingRows
