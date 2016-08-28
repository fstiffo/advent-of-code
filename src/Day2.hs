{-|
Module      : Day2
Description : Puzzle solutions for Advent of Code day 2
Copyright   : (c) Francesco Stiffoni, 2016
License     : GPL-3
Maintainer  : fstiffo@gmail.com
Stability   : experimental
Portability : POSIX

For details about the puzzles visit http://adventofcode.com/day/2
-}
module Day2
    (
    solution1, solution2
    ) where

import           Day2PuzzleInput               (input)
import           Text.ParserCombinators.Parsec

data Box = Box { l :: Int, w :: Int, h :: Int } deriving (Show)
type ElvesList = [Box]

elvesList :: Parser [Box]
elvesList = sepBy box eol

eol :: Parser String
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"

box :: Parser Box
box = do
    l <- many1 digit
    char 'x'
    w <- many1 digit
    char 'x'
    h <- many1 digit
    return Box {
      l = read l :: Int,
      w = read w :: Int,
      h = read h :: Int }

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

puzzleInput :: [Box]
puzzleInput =
  case regularParse elvesList input of
    Right r -> r
    Left r  -> []

wrappingPaper :: Box -> Int
wrappingPaper (Box l w h) =
  let sideAreas = [l * w, w * h, l * h]
      smallestArea = minimum sideAreas
  in  sum sideAreas * 2 + smallestArea

wrappingRibbon :: Box -> Int
wrappingRibbon (Box l w h) =
  let sidePerimeters = [l + w, w + h, l + h]
      smallestPerimeter = minimum sidePerimeters
      bowRibbon = l * w * h
  in  smallestPerimeter * 2 + bowRibbon


solution1 :: Int
solution1 = sum $ map wrappingPaper puzzleInput

solution2 :: Int
solution2 = sum $ map wrappingRibbon puzzleInput
