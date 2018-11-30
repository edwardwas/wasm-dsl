{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Wasm.Types where

import           Data.Singletons.TH
import           Data.Text          (Text)

$(singletons [d|
  data WasmType = I32 | I64 | F32 | F64
      deriving (Eq,Show)

  |])

wasmTypePrefix :: WasmType -> Text
wasmTypePrefix I32 = "i32"
wasmTypePrefix I64 = "i64"
wasmTypePrefix F32 = "f32"
wasmTypePrefix F64 = "f64"

data WasmPrimitive t where
  PrimI32 :: Int -> WasmPrimitive I32
  PrimI64 :: Integer -> WasmPrimitive I64
  PrimF32 :: Float -> WasmPrimitive F32
  PrimF64 :: Double -> WasmPrimitive F64

deriving instance Eq (WasmPrimitive t)
deriving instance Show (WasmPrimitive t)

class PromoteWasm a where
  type family AssociatedWasmType a :: WasmType
  promoteWasm :: a -> WasmPrimitive (AssociatedWasmType a)

instance PromoteWasm Int where
  type AssociatedWasmType Int = I32
  promoteWasm = PrimI32

instance PromoteWasm Integer where
  type AssociatedWasmType Integer = I64
  promoteWasm = PrimI64

instance PromoteWasm Float where
  type AssociatedWasmType Float = F32
  promoteWasm = PrimF32

instance PromoteWasm Double where
  type AssociatedWasmType Double = F64
  promoteWasm = PrimF64

withWasmPrimtive ::
       (forall a. (Num a, Show a, PromoteWasm a) =>
                      a -> b)
    -> WasmPrimitive t
    -> b
withWasmPrimtive f (PrimI32 n) = f n
withWasmPrimtive f (PrimI64 n) = f n
withWasmPrimtive f (PrimF32 n) = f n
withWasmPrimtive f (PrimF64 n) = f n

withWasmPrimtive2 ::
       (forall a. (Num a, Show a, PromoteWasm a, AssociatedWasmType a ~ t) =>
                      a -> a -> b)
    -> WasmPrimitive t
    -> WasmPrimitive t
    -> b
withWasmPrimtive2 f (PrimI32 a) (PrimI32 b) = f a b
withWasmPrimtive2 f (PrimI64 a) (PrimI64 b) = f a b
withWasmPrimtive2 f (PrimF32 a) (PrimF32 b) = f a b
withWasmPrimtive2 f (PrimF64 a) (PrimF64 b) = f a b
