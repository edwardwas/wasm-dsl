{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Wasm.Module where

import           Wasm.Function
import           Wasm.Instruction

import           Data.Singletons
import           Data.Text.Prettyprint.Doc

data SomeFunction args where
    SomeFunction
        :: (SingI res, MaybeConstraint SingI res)
        => Function inputs args res
        -> SomeFunction args

deriving instance Show (SomeFunction args)

data Module = Module {funcions :: [SomeFunction '[]]}
  deriving (Show)

prettyModule :: Module -> Doc ann
prettyModule (Module functions) =
    parens $
    vsep $
    "module" :
    indent 2 "(memory $0 1)" :
    map (\(SomeFunction f) -> indent 2 $ prettyFunction f) functions
