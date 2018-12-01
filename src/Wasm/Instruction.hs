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
    WasmLessThanOrEqual
        :: IsFloatWasmType t
        => WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just 'I32)
    WasmLessThan
        :: IsFloatWasmType t
        => WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just 'I32)
    WasmGreaterThanOrEqual
        :: IsFloatWasmType t
        => WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just 'I32)
    WasmGreaterThan
        :: IsFloatWasmType t
        => WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just 'I32)
    WasmLessThanOrEqualSigned
        :: IsIntWasmType t
        => WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just 'I32)
    WasmLessThanSigned
        :: IsIntWasmType t
        => WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just 'I32)
    WasmGreaterThanOrEqualSigned
        :: IsIntWasmType t
        => WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just 'I32)
    WasmGreaterThanSigned
        :: IsIntWasmType t
        => WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just 'I32)
    WasmLessThanOrEqualUnsigned
        :: IsIntWasmType t
        => WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just 'I32)
    WasmLessThanUnsigned
        :: IsIntWasmType t
        => WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just 'I32)
    WasmGreaterThanOrEqualUnsigned
        :: IsIntWasmType t
        => WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just t)
        -> WasmInstruction args ('Just 'I32)
    WasmGreaterThanUnsigned
        :: IsIntWasmType t
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

wasmOperator ::
       forall t args ann. (SingI t)
    => Text
    -> WasmInstruction args ('Just t)
    -> WasmInstruction args ('Just t)
    -> Doc ann
wasmOperator name a b =
    parens $
    vsep
        [ pretty (wasmTypePrefix (demote @t)) <> "." <>
          pretty name
        , indent 2 $ prettyWasmInstruction a
        , indent 2 $ prettyWasmInstruction b
        ]

prettyWasmInstruction ::
     forall t args ann. (SingI t, MaybeConstraint SingI t)
  => WasmInstruction args t
  -> Doc ann
prettyWasmInstruction (WasmConstant (prim :: WasmPrimitive s)) =
  parens $
  pretty (wasmTypePrefix (demote @s)) <> ".const" <+>
  (pretty $ show $ lowerWasm prim)
prettyWasmInstruction (WasmAdd a b) = wasmOperator "add" a b
prettyWasmInstruction (WasmMul a b) = wasmOperator "mul" a b
prettyWasmInstruction (WasmSub a b) = wasmOperator "sub" a b
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
prettyWasmInstruction (WasmBlock mName args) =
    parens $
    vsep
        (("block" <+> pretty (fromMaybe mempty mName)) :
         map (indent 2 . prettyWasmInstruction) args)
prettyWasmInstruction (WasmEq a b) =  wasmOperator "eq" a b
prettyWasmInstruction (WasmLessThanOrEqual a b) =  wasmOperator "le" a b
prettyWasmInstruction (WasmLessThan a b) =  wasmOperator "lt" a b
prettyWasmInstruction (WasmGreaterThanOrEqual a b) =  wasmOperator "ge" a b
prettyWasmInstruction (WasmGreaterThan a b) =  wasmOperator "gt" a b
prettyWasmInstruction (WasmLessThanOrEqualSigned a b) =  wasmOperator "le_s" a b
prettyWasmInstruction (WasmLessThanSigned a b) =  wasmOperator "lt_s" a b
prettyWasmInstruction (WasmGreaterThanOrEqualSigned a b) =  wasmOperator "ge_s" a b
prettyWasmInstruction (WasmGreaterThanSigned a b) =  wasmOperator "gt_s" a b
prettyWasmInstruction (WasmLessThanOrEqualUnsigned a b) =  wasmOperator "le_u" a b
prettyWasmInstruction (WasmLessThanUnsigned a b) =  wasmOperator "lt_u" a b
prettyWasmInstruction (WasmGreaterThanOrEqualUnsigned a b) =  wasmOperator "ge_u" a b
prettyWasmInstruction (WasmGreaterThanUnsigned a b) =  wasmOperator "gt_u" a b

flatternBlocks :: WasmInstruction args res -> WasmInstruction args res
flatternBlocks (WasmBlock na as) =
    let helper (WasmBlock nb bs)
          | nb == na = bs
          | otherwise = [WasmBlock nb bs]
        helper instr = [instr]
     in WasmBlock na (as >>= helper)
flatternBlocks instr = instr

appendHelper ::
       WasmInstruction args 'Nothing
    -> WasmInstruction args 'Nothing
    -> WasmInstruction args 'Nothing
appendHelper (WasmBlock na as) (WasmBlock nb bs)
    | na == nb = WasmBlock na (as <> bs)
    | otherwise = WasmBlock Nothing [WasmBlock na as, WasmBlock nb bs]
appendHelper a b = WasmBlock Nothing [a, b]

instance Semigroup (WasmInstruction args 'Nothing) where
  a <> b = flatternBlocks $ appendHelper a b
