module Clang.Nodes where

import Control.Lens ((&))
import Clang.FFI
import Clang.Types
import Data.ByteString (ByteString)

#include "clang-c/Index.h"

cursorNode :: Cursor -> Node
cursorNode c = c & case cursorKind c of
  (#const CXCursor_FunctionDecl)    -> pure FunctionDecl
  (#const CXCursor_ParmDecl)        -> ParmDecl <$> cursorSpelling
  (#const CXCursor_TypedefDecl)     -> pure TypedefDecl
  (#const CXCursor_TranslationUnit) -> pure TranslationUnit
  _ -> pure Unknown

data Node
  = FunctionDecl    --   8
  | ParmDecl { name :: ByteString }       --  10
  | TypedefDecl     --  20
  | TranslationUnit -- 300
  | Unknown
    deriving Show