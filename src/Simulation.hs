module Simulation where

import Board


-- same as zip3List, but is recursive to avoid adding an extra zero element in the beginning
zip3ListRecursive :: a -> [a] -> [[a]]
zip3ListRecursive zero (x : xs : xss : xsss) = [x, xs, xss] : zip3ListRecursive zero (xs : xss : xsss)
zip3ListRecursive zero [x, xs] = [[x, xs, zero]]
zip3ListRecursive _ _ = []

-- convert a list into a list of 3, joining neighboring elements into the same list
-- note that a zero element is provided
zip3List :: a -> [a] -> [[a]]
zip3List zero list@(x : xs : _) = [zero, x, xs] : zip3ListRecursive zero list
zip3List _ _ = []

-- compute the next state of the cell
reduceCell :: [Bool] -> Bool
reduceCell l =
  let s = sum (map fromEnum l) :: Int
   in (l !! 4 && (s == 3 || s == 4)) || (not (l !! 4) && s == 3)

-- add the current cell to a list of cells, keeping track of the length
appendCell :: [Bool] -> ([Bool], Int) -> ([Bool], Int)
appendCell cell (lst, num) =
  let c = reduceCell cell
   in (c : lst, num + 1)

-- compute the resulting row knowing the state and neighbors of each cell
reduceRow :: [[Bool]] -> Row
reduceRow row =
  let (list, num) = foldr appendCell ([], 0) row
   in Row num list

-- concatenate neighboring elements in groups of 3
zipRows :: [[Bool]] -> [[Bool]]
zipRows (x : xs : xss : xsss) = (x ++ xs ++ xss) : zipRows (xs : xss : xsss)
zipRows [x, xs] = [x ++ xs]
zipRows _ = []

-- combine three rows into one row of three elements
tripleRows :: [[Bool]] -> [[Bool]]
tripleRows [x:xs,y:ys,z:zs] = [x,y,z] : tripleRows [xs,ys,zs]
tripleRows _ = []

-- convert a list of rows to a 2d list containing a list of all neighbors of a cell and the cell itself
rowsToCells :: [Row] -> [[[Bool]]]
rowsToCells rows =
  let empty = repeat False
      newRows = map cells rows
   in map
        (zipRows . (replicate 3 False:) . tripleRows)
        (zip3List empty newRows)

-- given a list of rows, compute their state in the next generation
computeRows :: [Row] -> [Row]
computeRows [] = []
computeRows rows = map reduceRow (rowsToCells rows)

-- compute the next state of the board
next :: Board -> Board
next (Board _ _ rows) =
  let resultingRows = computeRows rows
   in Board (length resultingRows) (foldr (max . len) 0 resultingRows) resultingRows
