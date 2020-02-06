{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
-- |
-- Module:      Data.Aeson.Object.Generic
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- TODO: Module description.
module Data.Aeson.Object.Generic
    ( IsRecord
    )
  where

import GHC.Generics
    ( C
    , D
    , K1
    , M1
    , Meta(MetaSel)
    , Par1
    , Rec1
    , S
    , U1
    , type (:*:)
    , type (:+:)
    , type (:.:)
    )

import Data.Kind (Type)
import Data.Type.Bool (type (&&))


class IsRecord (f :: Type -> Type) (r :: Bool) | f -> r

instance
    ( IsRecord f lhs
    , IsRecord g rhs
    , r ~ (lhs && rhs)
    ) => IsRecord (f :*: g) r

instance {-# OVERLAPPING #-} IsRecord (M1 S ('MetaSel 'Nothing u s d) f) 'False
instance (IsRecord f r) => IsRecord (M1 S c f) r

instance (IsRecord f r) => IsRecord (M1 C c f) r
instance (IsRecord f r) => IsRecord (M1 D c f) r

instance IsRecord (f :+: g) 'False
instance IsRecord (f :.: g) 'True
instance IsRecord (K1 i c) 'True
instance IsRecord Par1 'True
instance IsRecord (Rec1 f) 'True
instance IsRecord U1 'False
