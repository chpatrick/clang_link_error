{-# LANGUAGE QuasiQuotes, TemplateHaskell, ForeignFunctionInterface #-}

module Clang.FFI
  ( createIndex
  , parseTranslationUnit, translationUnitCursor
  , cursorChildren, cursorKind, cursorSpelling
  ) where

import Control.Lens (Fold)
import Control.Lens.Internal (noEffect)
import Data.ByteString (packCString, ByteString)
import Foreign
import Foreign.C
import Data.Foldable
import Data.IORef
import qualified Data.Vector.Storable as VS
import qualified Language.C.Inline as C
import System.IO.Unsafe

import Clang.Context
import Clang.Types

C.context clangCtx
C.include "<stdlib.h>"
C.include "clang-c/Index.h"
C.include "../cbits/util.h"

foreign import ccall "&clang_disposeIndex"
  clang_disposeIndex :: FunPtr (CXIndex -> IO ())

createIndex :: IO ClangIndex
createIndex = do
  cxi <- [C.exp| CXIndex { clang_createIndex(0, 1) } |]
  ClangIndex <$> newForeignPtr clang_disposeIndex cxi

foreign import ccall "&clang_disposeTranslationUnit"
  clang_disposeTranslationUnit :: FunPtr (CXTranslationUnit -> IO ())

manageTranslationUnit :: ClangIndex -> CXTranslationUnit -> IO TranslationUnit
manageTranslationUnit idx ptr
  = TransUnit idx <$> newForeignPtr clang_disposeTranslationUnit ptr

parseTranslationUnit :: ClangIndex -> String -> [ String ] -> IO TranslationUnit
parseTranslationUnit idx path args =
  withForeignPtr (indexPtr idx) $ \cidx -> 
    withCString path $ \cPath -> do
      cArgs <- VS.fromList <$> traverse newCString args
      ctu <- [C.exp| CXTranslationUnit {
        clang_parseTranslationUnit(
          $(CXIndex cidx),
          $(char* cPath),
          $vec-ptr:(const char * const * cArgs), $vec-len:cArgs,
          NULL, 0,
          0)
        } |]
      traverse_ free $ VS.toList cArgs
      manageTranslationUnit idx ctu

translationUnitCursor :: TranslationUnit -> Cursor
translationUnitCursor tu =
  unsafePerformIO $
    withForeignPtr (translationUnitPtr tu) $ \ctu ->
      alloca $ \cp -> do
        [C.block| void { *$(CXCursor *cp) = clang_getTranslationUnitCursor($(CXTranslationUnit ctu)); } |]
        peekCursor tu cp

-- | Traverses over the children of a cursor. Compatible with "Control.Lens.Plated".
cursorChildren :: Fold Cursor Cursor
cursorChildren f c = unsafePerformIO $ do
  fRef <- newIORef noEffect
  let
    visitChild cp = do
      ch <- peekCursor (cursorUnit c) cp
      modifyIORef' fRef (*> f ch)
  with c $ \cp -> [C.exp| void {
      clang_visitChildren(
        *$(CXCursor *cp),
        visit_haskell,
        $fun:(void (*visitChild)(CXCursor*)))
    } |]
  readIORef fRef

unsafeWithCursor :: Cursor -> (Ptr Cursor -> IO a) -> a
unsafeWithCursor c f
  = unsafePerformIO $
    withForeignPtr (translationUnitPtr tu) $ \_ ->
      withForeignPtr (indexPtr idx) $ \_ ->
        with c f
  where
    tu = cursorUnit c
    idx = translationUnitIndex tu

-- | Converts a function with a CXString out parameter to one producing a ByteString.
withCXString :: (Ptr CXString -> IO ()) -> IO ByteString
withCXString f = allocaBytes cxStringSize $ \sp -> do
  f sp
  cs <- [C.exp| const char* { clang_getCString(*$(CXString *sp)) } |]
  s <- packCString cs
  [C.exp| void { clang_disposeString(*$(CXString *sp)) } |]
  return s

cursorSpelling :: Cursor -> ByteString
cursorSpelling c = unsafeWithCursor c $ \cp -> withCXString $ \sp ->
  [C.block| void {
    *$(CXString *sp) = clang_getCursorSpelling(*$(CXCursor *cp));
    } |]
