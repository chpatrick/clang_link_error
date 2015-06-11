module Main where

import Clang

main :: IO ()
main = do
  _ <- createIndex
  return ()
