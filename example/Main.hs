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

fibFunc :: RecursiveFunction '[ '( "n", 'F64)] '[] ('Just 'F64)
fibFunc fRef =
  Function "fib" True $
  let child x =
        CallFunctionInstr $ CallFunction fRef -$- #n - x
  in WasmIf (WasmEq @'F64 #n 1 + WasmEq @'F64 #n 2) 1 (child 1 + child 2)

testModule :: Module ()
testModule = do
  fibF <- moduleRecursiveFunction fibFunc
  _ <-
    moduleFunction $
    Function @'[] "main" True $ CallFunctionInstr $ CallFunction fibF -$- 10
  return ()

main :: IO ()
main = print $ prettyModule testModule
