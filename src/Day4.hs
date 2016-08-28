{-|
Module      : Day4
Description : Puzzle solutions for Advent of Code day 4
Copyright   : (c) Francesco Stiffoni, 2016
License     : GPL-3
Maintainer  : fstiffo@gmail.com
Stability   : experimental
Portability : POSIX

For details about the puzzles visit http://adventofcode.com/day/4
-}
module Day4
    (
    solution1, solution2
    ) where

import           Crypto.Hash
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Day4PuzzleInput       (input)

md5 :: BS.ByteString -> Digest MD5
md5 = hash

miningCondition :: String -> String -> Int -> Bool
miningCondition prefix key n =
  BS.pack prefix `BS.isPrefixOf` hex
  where
    s = BS.pack $ key ++ show n
    hex = digestToHexByteString $ md5 s

search :: (Int -> Bool) -> [Int] -> Int
search condition ns =
  case findIndex condition ns of
    Just i ->  i
    _      -> -1

solution1 :: Int
solution1 = search condition [0..]
  where condition = miningCondition "00000" input

solution2 :: Int
solution2 = search condition [0..]
  where condition = miningCondition "000000" input
