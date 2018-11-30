{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Wasm.Function
import           Wasm.Instruction
import           Wasm.Module
import           Wasm.Types

testInstr :: WasmInstruction '[ '( "x", F32)] Nothing
testInstr =
    WasmStore (WasmConstant $ PrimI32 0) $
    WasmAdd (WasmConstant $ PrimF32 10) $ #x

testFunc :: Function '[ '( "x", F32)] '[] Nothing
testFunc = Function "example" True (NamedParam @"x" @F32 :->: NoArgs) testInstr

doubleFunc :: Function '[ '( "x", I32)] '[] (Just I32)
doubleFunc =
    Function "double" False (NamedParam @"x" @I32 :->: NoArgs) $ WasmAdd #x #x

testModule =
  Module
    [ SomeFunction testFunc
    , SomeFunction doubleFunc
    , SomeFunction $ Function "main" True NoArgs $ WasmConstant $ PrimF32 4
    ]


main = print $ prettyModule testModule
