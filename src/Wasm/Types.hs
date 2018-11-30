{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
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

class SingI t =>
      IsWasmType (t :: WasmType) where
  type AssociatedHaskellType t :: *
  promoteWasm :: AssociatedHaskellType t -> WasmPrimitive t
  lowerWasm :: WasmPrimitive t -> AssociatedHaskellType t

instance (Num (AssociatedHaskellType t), IsWasmType t) =>
         Num (WasmPrimitive t) where
  a + b = promoteWasm (lowerWasm a + lowerWasm b)
  a * b = promoteWasm (lowerWasm a * lowerWasm b)
  a - b = promoteWasm (lowerWasm a - lowerWasm b)
  abs = promoteWasm . abs . lowerWasm
  signum = promoteWasm . signum . lowerWasm
  fromInteger = promoteWasm . fromInteger
  negate = promoteWasm . negate . lowerWasm

instance (Fractional (AssociatedHaskellType t), IsWasmType t) =>
         Fractional (WasmPrimitive t) where
  fromRational = promoteWasm . fromRational
  recip = promoteWasm . recip . lowerWasm
  a / b = promoteWasm (lowerWasm a / lowerWasm b)

instance IsWasmType I32 where
  type AssociatedHaskellType I32 = Int
  promoteWasm = PrimI32
  lowerWasm (PrimI32 n) = n

instance IsWasmType I64 where
  type AssociatedHaskellType I64 = Integer
  promoteWasm = PrimI64
  lowerWasm (PrimI64 n) = n

instance IsWasmType F32 where
  type AssociatedHaskellType F32 = Float
  promoteWasm = PrimF32
  lowerWasm (PrimF32 n) = n

instance IsWasmType F64 where
  type AssociatedHaskellType F64 = Double
  promoteWasm = PrimF64
  lowerWasm (PrimF64 n) = n
