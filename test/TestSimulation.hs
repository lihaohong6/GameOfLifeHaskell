module TestSimulation where

import Simulation
import Board

prop_next :: [[Bool]] -> Bool
prop_next b = let board = Board 0 0 (map makeRow b)
  in board == next board
