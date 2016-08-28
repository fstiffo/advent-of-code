{-|
Module      : Day6
Description : Puzzle solutions for Advent of Code day 6
Copyright   : (c) Francesco Stiffoni, 2016
License     : GPL-3
Maintainer  : fstiffo@gmail.com
Stability   : experimental
Portability : POSIX

For details about the puzzles visit http://adventofcode.com/day/6
-}
module Day6
(
solution1, solution2
) where

import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.Bits
import           Day6PuzzleInput               (input)
import           Text.ParserCombinators.Parsec

type CoordPair = (Int, Int)
type Operation = (Int -> Int)
data Instruction = Instruction { op          :: Operation,
                                 leftTop     :: CoordPair,
                                 rightBottom :: CoordPair }

on1 :: Operation
on1 _ = 1

off1 :: Operation
off1 _ = 0

toggle1 :: Operation
toggle1 n = xor n 1

on2 :: Operation
on2 n = n + 1

off2 :: Operation
off2 n = max 0 (n - 1)

toggle2 :: Operation
toggle2 n = n + 2

instruction :: Int -> Parser Instruction
instruction n = do
  op <- try (string "turn on") <|> try (string "turn off") <|> try (string "toggle")
  spaces
  l <- many1 digit
  char ','
  t <- many1 digit
  spaces
  string "through"
  spaces
  r <- many1 digit
  char ','
  b <- many1 digit
  return Instruction {
    op =
      case n of
        1 -> case op of
              "turn on"  -> on1
              "turn off" -> off1
              "toggle"   -> toggle1
        2 -> case op of
              "turn on"  -> on2
              "turn off" -> off2
              "toggle"   -> toggle2,
    leftTop = ( read l :: Int, read t :: Int ),
    rightBottom = ( read r :: Int, read b :: Int )
    }

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

parseInstruction :: Int -> String -> Instruction
parseInstruction n s =
  case regularParse (instruction n) s of
    Right i -> i
    Left i  -> Instruction id (0,0) (0,0)

puzzleInput :: [String]
puzzleInput = lines input

instructions1 = map (parseInstruction 1) puzzleInput
instructions2 = map (parseInstruction 2) puzzleInput

setup :: [Instruction] -> UArray (Int, Int) Int
setup instrs = runSTUArray $ do
  lights <- newArray ((0,0), (999,999)) 0
  forM_ instrs $ \(Instruction op (l, t) (r, b)) ->
    forM_ [(i,j) | i<-[l..r], j<-[t..b]] $ \(i, j) -> do
      l <- readArray lights (i, j)
      let l' = op l
      writeArray lights (i, j) l'
  return lights

solution1 :: Int
solution1 = sum $ elems $ setup instructions1

solution2 :: Int
solution2 = sum $ elems $ setup instructions2
