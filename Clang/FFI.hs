{-# LANGUAGE QuasiQuotes, TemplateHaskell, ForeignFunctionInterface #-}

module Clang.FFI where

import Foreign
import Foreign.C

type CXIndexImpl = CInt
type CXIndex = Ptr CXIndexImpl
newtype ClangIndex = ClangIndex { indexPtr :: ForeignPtr CXIndexImpl }

foreign import ccall "&clang_disposeIndex"
  clang_disposeIndex :: FunPtr (CXIndex -> IO ())

foreign import ccall safe "static inline_c_0_0743333ac0e2089afd635e6749e4d3ce11c9abae"
  inline_c_ffi_1627442442 :: IO CXIndex

createIndex :: IO ClangIndex
createIndex = do
  cxi <- inline_c_ffi_1627442442
  -- using newForeignPtr_ here works
  ClangIndex <$> newForeignPtr clang_disposeIndex cxi
