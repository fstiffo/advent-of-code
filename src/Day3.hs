{-|
Module      : Day3
Description : Puzzle solutions for Advent of Code day 3
Copyright   : (c) Francesco Stiffoni, 2016
License     : GPL-3
Maintainer  : fstiffo@gmail.com
Stability   : experimental
Portability : POSIX

For details about the puzzles visit http://adventofcode.com/day/3
-}
module Day3
    (
    solution1, solution2
    ) where

import           Data.List
import           Day3PuzzleInput               (input)
import           Text.ParserCombinators.Parsec

type Home = (Int, Int)
type Move = Home -> Home
type Path = [Home]

north :: Move
north (x, y) = (x, y + 1)

south :: Move
south (x, y) = (x, y - 1)

east :: Move
east (x, y) = (x + 1, y)

west :: Move
west (x, y) = (x - 1, y)

move :: Parser Move
move = do
  m <- oneOf "^v<>"
  return $
    case m of
      '^' -> north
      'v' -> south
      '>' -> east
      '<' -> west
      _   -> id

moves :: Parser [Move]
moves = many1 move

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

puzzleInput :: [Move]
puzzleInput =
  case regularParse moves input of
    Right r -> r
    Left r  -> []

start :: Path
start = [(0, 0)]

extend :: Path -> Move -> Path
extend p m = p ++ [m $ last p]

homes :: Path -> [Move] -> Path
homes p [] = p
homes p (m:ms) =
  let nextHome = m $ last p
      p' = p ++ [nextHome]
  in  homes p' ms

split :: [a] -> ([a], [a])
split = foldr (\x (ys, zs) -> (x : zs, ys)) ([], [])

(santaMoves, robotMoves) = split puzzleInput

solution1 :: Int
solution1 = length $ nub $ homes start puzzleInput

solution2 :: Int
solution2 =
  length $ nub $ homes start santaMoves ++ homes start robotMoves
