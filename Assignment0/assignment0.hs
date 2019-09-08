module Main where

  import Data.List

  -- Reverses each line of provided strings and inserts "/" between each line
  main = interact (intercalate " / " . map reverse . lines)