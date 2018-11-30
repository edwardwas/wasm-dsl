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

testModule :: Module ()
testModule = do
  doubleF <- moduleFunction doubleFunction
  _ <-
    moduleFunction $
    Function "main" True NoArgs $
    CallFunctionInstr $
    ApplyInstruction (WasmConstant $ PrimF32 22.3) $ CallFunction doubleF
  return ()

main = print $ prettyModule testModule
