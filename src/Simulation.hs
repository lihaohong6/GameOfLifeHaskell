module Simulation where

import Board

zip3ListRecursive :: a -> [a] -> [[a]]
zip3ListRecursive zero (x : xs : xss : xsss) = [x, xs, xss] : zip3ListRecursive zero (xs : xss : xsss)
zip3ListRecursive zero [x, xs] = [[x, xs, zero]]
zip3ListRecursive _ _ = []

zip3List :: a -> [a] -> [[a]]
zip3List zero list@(x : xs : _) = [zero, x, xs] : zip3ListRecursive zero list
zip3List _ _ = []

reduceCell :: [Bool] -> Bool
reduceCell l =
  let s = sum (map fromEnum l) :: Int
   in (l !! 4 && (s == 3 || s == 4)) || (not (l !! 4) && s == 3)

appendCell :: [Bool] -> ([Bool], Int) -> ([Bool], Int)
appendCell cell (lst, num) =
  let c = reduceCell cell
   in (c : lst, num + 1)

reduceRow :: [[Bool]] -> Row
reduceRow row =
  let (list, num) = foldr appendCell ([], 0) row
   in Row num list

zipRows :: [[Bool]] -> [[Bool]]
zipRows (x : xs : xss : xsss) = (x ++ xs ++ xss) : zipRows (xs : xss : xsss)
zipRows [x, xs] = [x ++ xs]
zipRows _ = []

tripleRows :: [[Bool]] -> [[Bool]]
tripleRows [x:xs,y:ys,z:zs] = [x,y,z] : tripleRows [xs,ys,zs]
tripleRows _ = []


rowsToCells :: [Row] -> [[[Bool]]]
rowsToCells rows =
  let empty = repeat False
      newRows = map cells rows
   in map
        (zipRows . (replicate 3 False:) . tripleRows)
        (zip3List empty newRows)

computeRows :: [Row] -> [Row]
computeRows [] = []
computeRows rows = map reduceRow (rowsToCells rows)

next :: Board -> Board
next (Board _ _ rows) =
  let resultingRows = computeRows rows
   in Board (length resultingRows) (foldr (max . len) 0 resultingRows) resultingRows
