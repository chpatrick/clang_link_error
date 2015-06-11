{-# LANGUAGE EmptyDataDecls #-}

module Clang.Types where

#include "clang-c/Index.h"

import Data.Void
import Foreign
import Foreign.C

-- Opaque C types
data CXString
cxStringSize :: Int
cxStringSize = #size CXString

data CXIndexImpl
data CXTranslationUnitImpl

-- Pointers to C types (as used in the C API)
type CXIndex = Ptr CXIndexImpl
type CXTranslationUnit = Ptr CXTranslationUnitImpl

-- Haskell types (with finalizers and references to keep their parent structures alive)
newtype ClangIndex = ClangIndex { indexPtr :: ForeignPtr CXIndexImpl }

data TranslationUnit = TransUnit
  { translationUnitIndex :: ClangIndex
  , translationUnitPtr :: ForeignPtr CXTranslationUnitImpl
  }

type CXCursorKind = CInt 

data Cursor = Cursor
  { cursorUnit :: TranslationUnit
  , cursorKind :: CXCursorKind
  , cursorXData :: CInt
  , cursorData :: [ Ptr Void ]
  }

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable Cursor where
  peek = fail "Use peekCursor to peek a Cursor."
  poke cp c
    =  ((#poke CXCursor, kind) cp) (cursorKind c)
    *> ((#poke CXCursor, xdata) cp) (cursorXData c)
    *> pokeArray ((#ptr CXCursor, data) cp) (cursorData c)
  sizeOf _ = #size CXCursor
  alignment _ = #alignment CXCursor

peekCursor :: TranslationUnit -> Ptr Cursor -> IO Cursor
peekCursor u cp
  = Cursor u
  <$> (#peek CXCursor, kind) cp
  <*> (#peek CXCursor, xdata) cp
  <*> peekArray 3 ((#ptr CXCursor, data) cp)
