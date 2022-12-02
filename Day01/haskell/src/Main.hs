module Main (main) where

import Data.Char (isSpace)
import Data.List (sort)
import Data.List.Split (splitWhen)

part1 :: [String] -> Int
part1 = head . reverse . sort . map (sum . map read) . splitWhen (all isSpace)

part2 :: [String] -> Int
part2 = sum . take 3 . reverse . sort . map (sum . map read) . splitWhen (all isSpace)

main :: IO ()
main = do
  input <- lines <$> readFile "/home/erik/src/repos/unmango/aoc-2022/Day01/input.txt"
  print (part1 input)
  print (part2 input)
