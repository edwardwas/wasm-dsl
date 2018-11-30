{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Wasm.Module where

import           Wasm.Function
import           Wasm.Instruction

import           Control.Monad.Fix
import           Control.Monad.Writer
import           Data.Singletons
import           Data.Text.Prettyprint.Doc

data SomeFunction where
  SomeFunction
    :: (MaybeConstraint SingI res, SingI res)
    => Function inputs args res
    -> SomeFunction

newtype Module a = Module (Writer [SomeFunction] a)
  deriving (Functor,Applicative,Monad,MonadFix)

moduleFunction ::
     (MaybeConstraint SingI res, SingI res)
  => Function inputs args res
  -> Module (FunctionRef inputs as res)
moduleFunction f@(Function name _ _ _) =
  Module (FunctionRef name <$ tell (pure $ SomeFunction f))

prettyModule :: Module () -> Doc ann
prettyModule (Module act) =
  let helper funcs =
        parens $
        vsep
          ("module" :
           indent 2 (parens $ "memory $0 1") :
           map (\(SomeFunction f) -> indent 2 $ prettyFunction f) funcs)
  in helper $ execWriter act
