module Simulation where

import Board

data Triple = Triple Bool Bool Bool
  deriving (Show, Eq)

emptyTriple :: Triple
emptyTriple = Triple False False False

tupleToTriple :: (Bool, Bool, Bool) -> Triple
tupleToTriple (a, b, c) = Triple a b c

data Cell = Cell Triple Triple Triple
  deriving (Show, Eq)

tupleToCell :: (Triple, Triple, Triple) -> Cell
tupleToCell (a, b, c) = Cell a b c

zip3List :: a -> [a] -> [(a, a, a)]
zip3List zero (x : xs : xss : xsss) = (x, xs, xss) : zip3List zero (xs : xss : xsss)
zip3List zero (x : xs : _) = [(x, xs, zero)]
zip3List _ _ = []

zipRow :: [Triple] -> [Cell]
zipRow list = map tupleToCell (zip3List emptyTriple list)

reduceCell :: Cell -> Bool
reduceCell (Cell (Triple a b c) (Triple d e f) (Triple g h i)) =
  let s = sum (map (\x -> if x then 1 else 0) [a, b, c, d, e, f, g, h, i]) :: Int
   in (e && (s == 2 || s == 3)) || (not e && s == 3)

appendCell :: Cell -> ([Bool], Int) -> ([Bool], Int)
appendCell cell (lst, num) =
  let c = reduceCell cell
   in if num > 0
        then (c : lst, num + 1)
        else if c then ([c], 1) else (lst, 0)

reduceRow :: [Cell] -> Row
reduceRow row =
  let (list, num) = foldr appendCell ([], 0) row
   in Row num (num > 0) list

zipRows :: (Row, Row, Row) -> [Triple]
zipRows (Row _ _ r1, Row _ _ r2, Row _ _ r3) = map tupleToTriple (zip3 r1 r2 r3)

computeRows :: [Row] -> [Row]
computeRows rows =
  let newRows =
        map
          zipRows
          ( zip3
              (emptyRow : emptyRow : rows)
              (emptyRow : rows ++ [emptyRow])
              (drop 1 rows ++ [emptyRow, emptyRow])
          )
   in map (reduceRow . zipRow) newRows

next :: Board -> Board
next (Board _ _ rows) =
  let resultingRows = computeRows rows
   in Board (length resultingRows) (foldr (max . len) 0 resultingRows) resultingRows
