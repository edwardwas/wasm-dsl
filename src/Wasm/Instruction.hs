{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Wasm.Instruction where

import           Data.Singletons
import           Data.Singletons.Decide
import           Data.Singletons.Prelude    hiding (Elem)
import           Data.Singletons.Prelude.Eq
import           Data.Text                  (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Type.Equality
import           GHC.Exts
import           GHC.OverloadedLabels
import           GHC.TypeLits

import           Wasm.Types

data NamedParam (name :: Symbol) (t :: WasmType) where
    NamedParam :: (KnownSymbol name, SingI t) => NamedParam name t

deriving instance Show (NamedParam n t)
deriving instance Eq (NamedParam n t)

instance (SingI t, KnownSymbol name', name ~ name') =>
         IsLabel name (NamedParam name' t) where
    fromLabel = NamedParam

type family Elem a as :: Constraint where
  Elem a (a ': _) = ()
  Elem a (_ ': as) = (Elem a as)

data WasmInstruction args t where
    WasmConstant :: WasmPrimitive t -> WasmInstruction args (Just t)
    WasmAdd
        :: WasmInstruction args (Just t)
        -> WasmInstruction args (Just t)
        -> WasmInstruction args (Just t)
    WasmStore
        :: SingI t
        => WasmInstruction args (Just I32)
        -> WasmInstruction args (Just t)
        -> WasmInstruction args Nothing
    GetLocal
        :: (KnownSymbol name, Elem '(name,t) args, SingI t)
        => NamedParam name t
        -> WasmInstruction args (Just t)

instance (SingI t, KnownSymbol name, Elem '( name, t) args) =>
         IsLabel name (WasmInstruction args (Just t)) where
    fromLabel = GetLocal (NamedParam :: NamedParam name t)

deriving instance Show (WasmInstruction args t)

instance Eq (WasmInstruction args t) where
    WasmConstant a == WasmConstant b = a == b
    WasmAdd a b == WasmAdd c d = a == c && b == d
    WasmStore a (b :: WasmInstruction args (Just x)) == WasmStore c (d :: WasmInstruction args (Just y)) =
        case (sing :: Sing x) %~ (sing :: Sing y) of
            Proved ref ->
                case apply (Refl @Just) ref of
                    Refl -> b == d && a == c
            Disproved _ -> False
    GetLocal (p1 :: NamedParam name1 t1) == GetLocal (p2 :: NamedParam name2 t2) =
        case (sing :: Sing name1) %~ (sing :: Sing name2) of
            Proved _ ->
                case (sing :: Sing t1) %~ (sing :: Sing t2) of
                    Proved _    -> True
                    Disproved _ -> False
            Disproved _ -> False
    _ == _ = False

type family MaybeConstraint c mx :: Constraint where
    MaybeConstraint c (Just x) = (c x)
    MaybeConstraint _ Nothing = ()

prettyWasmInstruction ::
       MaybeConstraint SingI t => WasmInstruction args t -> Doc ann
prettyWasmInstruction (WasmConstant (prim :: WasmPrimitive s)) =
    parens $
    pretty (wasmTypePrefix (demote @s)) <> ".const" <+>
    withWasmPrimtive (pretty . show) prim
prettyWasmInstruction (WasmAdd (a :: WasmInstruction args (Just s)) b) =
    parens $
    vsep
        [ pretty (wasmTypePrefix (demote @s)) <> ".add"
        , indent 2 $ prettyWasmInstruction a
        , indent 2 $ prettyWasmInstruction b
        ]
prettyWasmInstruction (WasmStore addr (val :: WasmInstruction args (Just s))) =
    parens $
    vsep
        [ pretty (wasmTypePrefix (demote @s)) <> ".store"
        , indent 2 $ prettyWasmInstruction addr
        , indent 2 $ prettyWasmInstruction val
        ]
prettyWasmInstruction (GetLocal (_ :: NamedParam name s)) =
    parens $ "get_local $" <> pretty (symbolVal (Proxy @name))
