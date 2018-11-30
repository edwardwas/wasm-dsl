{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Wasm.Function where

import           Wasm.Instruction
import           Wasm.Types

import           Data.Singletons
import           Data.Singletons.Prelude   hiding (Elem)
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc
import           GHC.TypeLits

type family Append as bs where
  Append '[]  bs = bs
  Append (a ': as) bs = a ': (Append as bs)

type family RemoveFromList a as where
  RemoveFromList a (a ': as) = as
  RemoveFromList a (b ': as) = b ': RemoveFromList a as

type family DiffList as bs where
  DiffList as '[] = as
  DiffList as (b ': bs) = RemoveFromList b (DiffList as  bs)

data ArgList ts where
    NoArgs :: ArgList '[]
    (:->:) :: NamedParam n t -> ArgList ts -> ArgList ('( n, t) ': ts)

prettyArgList :: ArgList ts -> Doc ann
prettyArgList NoArgs = mempty
prettyArgList ((NamedParam :: NamedParam n t) :->: as) =
    parens
        ("param" <+>
         "$" <> pretty (symbolVal (Proxy @n)) <+>
         pretty (wasmTypePrefix (demote @t))) <+>
    prettyArgList as


deriving instance Eq (ArgList ts)
deriving instance Show (ArgList ts)

infixr 4 :->:

data Function inputs args res where
    Function
        :: Text
        -> Bool
        -> ArgList inputs
        -> WasmInstruction (Append inputs args) res
        -> Function inputs args res

deriving instance Show (Function inputs args res)

prettyFunction ::
       forall args res ann inputs. (MaybeConstraint SingI res, SingI res)
    => Function inputs args res
    -> Doc ann
prettyFunction (Function name export args body) =
    let nameDec =
            if export
                then parens ("export" <+> dquotes (pretty name))
                else "$" <> pretty name
        resultSection =
            case sing :: Sing res of
                SNothing   -> mempty
                SJust SI32 -> parens $ "result i32"
                SJust SI64 -> parens $ "result i64"
                SJust SF32 -> parens $ "result f32"
                SJust SF64 -> parens $ "result f64"
     in parens $
        vsep
            [ "func" <+> nameDec <+> prettyArgList args <+> resultSection
            , indent 2 $ prettyWasmInstruction body
            ]
