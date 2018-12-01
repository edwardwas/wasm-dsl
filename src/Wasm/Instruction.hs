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

import           Data.Maybe                (fromMaybe)
import           Data.Singletons
import           Data.Singletons.Prelude   hiding (Elem)
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc
import           GHC.Exts
import           GHC.OverloadedLabels
import           GHC.TypeLits

import           Wasm.Types

resultSection :: forall res ann . Sing (res :: Maybe WasmType) -> Doc ann
resultSection s =
    case s :: Sing res of
        SNothing   -> mempty
        SJust SI32 -> parens $ "result i32"
        SJust SI64 -> parens $ "result i64"
        SJust SF32 -> parens $ "result f32"
        SJust SF64 -> parens $ "result f64"

data FunctionRef (inputs :: [(Symbol, WasmType)]) (args :: [(Symbol, WasmType)]) (res :: Maybe WasmType) where
    FunctionRef :: Text -> FunctionRef inputs args res

deriving instance Show (FunctionRef inputs args res)
deriving instance Eq (FunctionRef inputs args res)

data CallFunction inputs args res where
  CallFunction :: FunctionRef inputs args res -> CallFunction inputs args res
  ApplyInstruction
    :: (KnownSymbol name, SingI i, (t ~ i))
    => WasmInstruction args ('Just t)
    -> CallFunction ('( name, i) ': is) args res
    -> CallFunction is args res

(-$-) ::
     (KnownSymbol name, SingI i)
  => CallFunction ('( name, i) ': is) args res
  -> WasmInstruction args ('Just i)
  -> CallFunction is args res
(-$-) cf i = ApplyInstruction i cf

infixr 4 -$-

prettyCallFunctionInputs :: CallFunction inputs args res -> [Doc ann]
prettyCallFunctionInputs (CallFunction _) = []
prettyCallFunctionInputs (ApplyInstruction wi cf) =
  prettyWasmInstruction wi : prettyCallFunctionInputs cf

callFunctionName :: CallFunction inputs args res -> Text
callFunctionName (CallFunction (FunctionRef name)) = name
callFunctionName (ApplyInstruction _ cf)           = callFunctionName cf

prettyCallFunction :: CallFunction inputs args res -> Doc ann
prettyCallFunction cf =
  parens $
  vsep
    (("call $" <> pretty (callFunctionName cf)) :
     map (indent 2) (reverse $ prettyCallFunctionInputs cf))


deriving instance Show (CallFunction inputs args res)

instance SingI inputs => Eq (CallFunction inputs args res) where
  CallFunction fr1 == CallFunction fr2 = fr1 == fr2
  _ == _ = undefined

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
    WasmConstant
        :: (Show (AssociatedHaskellType t), IsWasmType t)
        => WasmPrimitive t
        -> WasmInstruction args ('Just t)
    WasmAdd
        :: WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
    WasmSub
        :: WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
    WasmMul
        :: WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
    WasmStore
        :: SingI t
        => WasmInstruction args ('Just 'I32)
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args 'Nothing
    GetLocal
        :: (KnownSymbol name, Elem '( name, t) args, SingI t)
        => NamedParam name t
        -> WasmInstruction args ('Just t)
    SetLocal
        :: (KnownSymbol name, Elem '( name,t) args, SingI t)
        => NamedParam name t
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args 'Nothing
    TeeLocal
        :: (KnownSymbol name, Elem '( name,t) args, SingI t)
        => NamedParam name t
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
    CallFunctionInstr :: CallFunction '[] args res -> WasmInstruction args res
    WasmIf
        :: WasmInstruction args ('Just 'I32)
        -> WasmInstruction args t
        -> WasmInstruction args t
        -> WasmInstruction args t
    WasmEq
        :: SingI t
        => WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just 'I32)
    WasmBlock
        :: Maybe Text
        -> [WasmInstruction args 'Nothing]
        -> WasmInstruction args 'Nothing
    WasmLessThanOrEqal
        :: IsFloatWasmType t
        => WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just 'I32)
    WasmLessThan
        :: IsFloatWasmType t
        => WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just 'I32)
    WasmGreaterThanOrEqal
        :: IsFloatWasmType t
        => WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just 'I32)
    WasmGreaterThan
        :: IsFloatWasmType t
        => WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just 'I32)

instance (SingI t, KnownSymbol name, Elem '( name, t) args) =>
         IsLabel name (WasmInstruction args ('Just t)) where
    fromLabel = GetLocal (NamedParam :: NamedParam name t)

instance ( Show (AssociatedHaskellType t)
         , IsWasmType t
         , Num (AssociatedHaskellType t)
         ) =>
         Num (WasmInstruction args ('Just t)) where
  a + b = WasmAdd a b
  a * b = WasmMul a b
  a - b = WasmSub a b
  fromInteger = WasmConstant . fromInteger
  abs = undefined
  signum = undefined

deriving instance Show (WasmInstruction args t)

instance Eq (WasmInstruction args t) where
    a == b = show a == show b

type family MaybeConstraint c mx :: Constraint where
    MaybeConstraint c ('Just x) = (c x)
    MaybeConstraint _ 'Nothing = ()

prettyWasmInstruction ::
     forall t args ann. (SingI t, MaybeConstraint SingI t)
  => WasmInstruction args t
  -> Doc ann
prettyWasmInstruction (WasmConstant (prim :: WasmPrimitive s)) =
  parens $
  pretty (wasmTypePrefix (demote @s)) <> ".const" <+>
  (pretty $ show $ lowerWasm prim)
prettyWasmInstruction (WasmAdd (a :: WasmInstruction args ('Just s)) b) =
  parens $
  vsep
    [ pretty (wasmTypePrefix (demote @s)) <> ".add"
    , indent 2 $ prettyWasmInstruction a
    , indent 2 $ prettyWasmInstruction b
    ]
prettyWasmInstruction (WasmMul (a :: WasmInstruction args ('Just s)) b) =
  parens $
  vsep
    [ pretty (wasmTypePrefix (demote @s)) <> ".mul"
    , indent 2 $ prettyWasmInstruction a
    , indent 2 $ prettyWasmInstruction b
    ]
prettyWasmInstruction (WasmSub (a :: WasmInstruction args ('Just s)) b) =
  parens $
  vsep
    [ pretty (wasmTypePrefix (demote @s)) <> ".sub"
    , indent 2 $ prettyWasmInstruction a
    , indent 2 $ prettyWasmInstruction b
    ]
prettyWasmInstruction (WasmStore addr (val :: WasmInstruction args ('Just s))) =
  parens $
  vsep
    [ pretty (wasmTypePrefix (demote @s)) <> ".store"
    , indent 2 $ prettyWasmInstruction addr
    , indent 2 $ prettyWasmInstruction val
    ]
prettyWasmInstruction (GetLocal (_ :: NamedParam name s)) =
  parens $ "get_local $" <> pretty (symbolVal (Proxy @name))
prettyWasmInstruction (SetLocal (_ :: NamedParam name s) arg) =
    parens $
    vsep
        [ "set_local $" <> pretty (symbolVal (Proxy @name))
        , indent 2 $ parens $ prettyWasmInstruction arg
        ]
prettyWasmInstruction (TeeLocal (_ :: NamedParam name s) arg) =
    parens $
    vsep
        [ "tee_local $" <> pretty (symbolVal (Proxy @name))
        , indent 2 $ parens $ prettyWasmInstruction arg
        ]
prettyWasmInstruction (CallFunctionInstr cf) = prettyCallFunction cf
prettyWasmInstruction (WasmIf predicate a b) =
  parens $
  vsep
    [ "if" <+> resultSection (Sing :: Sing t)
    , indent 2 $ prettyWasmInstruction predicate
    , indent 2 $ parens $ vsep ["then", indent 2 $ prettyWasmInstruction a]
    , indent 2 $ parens $ vsep ["else", indent 2 $ prettyWasmInstruction b]
    ]
prettyWasmInstruction (WasmEq (a ::WasmInstruction args ('Just s)) b) =
  parens $
  vsep
    [ pretty (wasmTypePrefix (demote @s)) <> ".eq"
    , indent 2 $ prettyWasmInstruction a
    , indent 2 $ prettyWasmInstruction b
    ]
prettyWasmInstruction (WasmBlock mName args) =
    parens $
    vsep
        (("block" <+> pretty (fromMaybe "void" mName)) :
         map (indent 2 . prettyWasmInstruction) args)
prettyWasmInstruction (WasmLessThanOrEqal (a :: WasmInstruction args ('Just s)) b) =
    parens $
    vsep
        [ pretty (wasmTypePrefix (demote @s)) <> ".le"
        , indent 2 $ prettyWasmInstruction a
        , indent 2 $ prettyWasmInstruction b
        ]
prettyWasmInstruction (WasmLessThan (a :: WasmInstruction args ('Just s)) b) =
    parens $
    vsep
        [ pretty (wasmTypePrefix (demote @s)) <> ".lt"
        , indent 2 $ prettyWasmInstruction a
        , indent 2 $ prettyWasmInstruction b
        ]
prettyWasmInstruction (WasmGreaterThanOrEqal (a :: WasmInstruction args ('Just s)) b) =
    parens $
    vsep
        [ pretty (wasmTypePrefix (demote @s)) <> ".ge"
        , indent 2 $ prettyWasmInstruction a
        , indent 2 $ prettyWasmInstruction b
        ]
prettyWasmInstruction (WasmGreaterThan (a :: WasmInstruction args ('Just s)) b) =
    parens $
    vsep
        [ pretty (wasmTypePrefix (demote @s)) <> ".gt"
        , indent 2 $ prettyWasmInstruction a
        , indent 2 $ prettyWasmInstruction b
        ]

instance Semigroup (WasmInstruction args 'Nothing) where
    a <> b = WasmBlock Nothing [a, b]
