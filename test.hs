module Main where

import Clang
import Control.Lens
import Data.ByteString.Char8 (unpack)
import Data.Tree

showCursor :: Cursor -> String
showCursor c = show (cursorNode c) ++ " (" ++ show (cursorKind c) ++ ")"

main :: IO ()
main = do
  idx <- createIndex
  tu <- parseTranslationUnit idx "test.c" []
  let ast = unfoldTree (\c -> ( showCursor c, toListOf cursorChildren c )) (translationUnitCursor tu)
  putStr $ drawTree ast