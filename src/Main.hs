{-|
Module      : Main
Description : Puzzle solutions for Advent of Code
Copyright   : (c) Francesco Stiffoni, 2016
License     : GPL-3
Maintainer  : fstiffo@gmail.com
Stability   : experimental
Portability : POSIX

For details about the puzzles visit http://adventofcode.com/
-}

module Main where

import           Day2
import           Day3
import           Day4
import           Day5
import           Day6
import           Text.Printf

printSolutions :: Int -> Int -> Int -> IO ()
printSolutions d s1 s2 = do
  printf "Advent of Code Day %d\n" d
  printf "First half puzzle answer is %d\n" s1
  printf "Second half puzzle answer is %d\n" s2
  printf "--------------------------------------\n"


main :: IO ()
main = do
  -- printSolutions 2 Day2.solution1 Day2.solution2
  -- printSolutions 3 Day3.solution1 Day3.solution2
  -- printSolutions 4 Day4.solution1 Day4.solution2
  -- printSolutions 5 Day5.solution1 Day5.solution2
  printSolutions 6 Day6.solution1 Day6.solution2
