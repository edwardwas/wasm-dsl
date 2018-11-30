{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import           Wasm.Function
import           Wasm.Instruction
import           Wasm.Module
import           Wasm.Types

import           Control.Monad.Fix

doubleFunction :: Function '[ '( "x", F32)] '[] (Just F32)
doubleFunction =
  Function "double" True (NamedParam :->: NoArgs) $
  WasmAdd (GetLocal #x) (GetLocal #x)

powFunction ::
     FunctionRef '[ '( "n", I32), '( "x", F32)] '[ '( "n", I32), '( "x", F32)] (Just F32)
  -> Function '[ '( "n", I32), '( "x", F32)] '[] (Just F32)
powFunction fRef =
  Function "pow" False (NamedParam :->: NamedParam :->: NoArgs) $
  WasmIf
    (WasmEq #n (WasmConstant $ PrimI32 1))
    #x
    (#x *
     (CallFunctionInstr $
      ApplyInstruction #x $ ApplyInstruction (#n - 1) $ CallFunction fRef))

fibFunc ::
     FunctionRef '[ '( "n", F64)] '[ '( "n", F64)] (Just F64)
  -> Function '[ '( "n", F64)] '[] (Just F64)
fibFunc fRef =
  Function "fib" True (NamedParam :->: NoArgs) $
  let child x =
        CallFunctionInstr $ ApplyInstruction (#n - x) $ CallFunction fRef
  in WasmIf ((WasmEq @F64 #n 1) + (WasmEq @F64 #n 2)) 1 (child 1 + child 2)

testModule :: Module ()
testModule = do
  fibF <- mfix (moduleFunction . fibFunc)
  _ <-
    moduleFunction $
    Function "main" True NoArgs $
    CallFunctionInstr $
    ApplyInstruction 6 $ CallFunction fibF
  return ()

main = print $ prettyModule testModule
