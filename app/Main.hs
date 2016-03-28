module Main where

import System.Random (newStdGen)

import Lib

main :: IO ()
main = do
  gen <- newStdGen
  let w = 20
      h = 12
      maze = createMaze gen w h
      soln = solveMaze maze (1, 1) (w, h)
  putStr . renderMaze maze $ soln
