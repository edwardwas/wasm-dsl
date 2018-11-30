{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import           Wasm.Function
import           Wasm.Instruction
import           Wasm.Module
import           Wasm.Types

doubleFunction :: Function '[ '( "x", F32)] '[] (Just F32)
doubleFunction =
  Function "double" True (NamedParam :->: NoArgs) $
  WasmAdd (GetLocal #x) (GetLocal #x)

powFunction :: Function '[ '( "n", I32), '( "x", F32)] '[] (Just F32)
powFunction =
  Function "pow" False (NamedParam :->: NamedParam :->: NoArgs) $
  WasmIf
    (WasmEq #n (WasmConstant $ PrimI32 1))
    #x
    (WasmConstant 3.2)

testModule :: Module ()
testModule = do
  doubleF <- moduleFunction doubleFunction
  powF <- moduleFunction powFunction
  _ <-
    moduleFunction $
    Function "main" True NoArgs $
    CallFunctionInstr $
    ApplyInstruction (WasmConstant 2) $
    ApplyInstruction (WasmConstant 1) $ CallFunction powF
  return ()

main = print $ prettyModule testModule
