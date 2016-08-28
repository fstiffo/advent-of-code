{-|
Module      : Day5
Description : Puzzle solutions for Advent of Code day 5
Copyright   : (c) Francesco Stiffoni, 2016
License     : GPL-3
Maintainer  : fstiffo@gmail.com
Stability   : experimental
Portability : POSIX

For details about the puzzles visit http://adventofcode.com/day/5
-}
module Day5
(
solution1, solution2
) where

import           Data.List
import           Day5PuzzleInput               (input)

data Properties1 = Props1 { wovels :: Int, twice :: Bool, bad :: Bool }

check1 :: (Properties1, Char) -> Char -> (Properties1, Char)
check1 (Props1 w t b, prev) next =
  let wovels = if next `elem` "aeiou" then w + 1 else w
      twice = prev == next || t
      bad = prev : [next] `elem` ["ab", "cd", "pq", "xy"] ||  b
  in (Props1 wovels twice bad, next)

satisfy1 :: Properties1 -> Bool
satisfy1 (Props1 wovels twice bad) = wovels >= 3 && twice && not bad

nice1 :: String -> Bool
nice1 s = satisfy1 $ fst $ foldl check1 acc s
  where acc = (Props1 0 False False, '!')

data Properties2 = Props2 { pair :: Bool, sandwich :: Bool }
type Accumulator = (Properties2, (Char, Char, String))

check2 :: Accumulator -> Char -> Accumulator
check2 (Props2 p s, (prevPrev, prev, c : cs)) next =
  let pair = (prevPrev : [prev]) `isInfixOf` (c : cs) || p
      sandwich = prevPrev == next || s
  in (Props2 pair sandwich, (prev, next, cs))

satisfy2 :: Properties2 -> Bool
satisfy2 (Props2 pair sandwich) = pair && sandwich

nice2 :: String -> Bool
nice2 s = satisfy2 $ fst $ foldl check2 acc s
  where acc = (Props2 False False, ('!', '?', s))

puzzleInput :: [String]
puzzleInput = lines input

solution1 :: Int
solution1 = length $ filter id $ map nice1 puzzleInput

solution2 :: Int
solution2 = length $ filter id $ map nice2 puzzleInput
