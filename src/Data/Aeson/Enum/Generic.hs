{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
-- |
-- Module:      Data.Aeson.Enum.Generic
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- TODO: Module description.
module Data.Aeson.Enum.Generic
--  (
--  )
  where

import GHC.Generics
    ( K1
    , M1
    , Par1
    , Rec1
    , U1
    , type (:*:)
    , type (:+:)
    , type (:.:)
    )

import Data.Kind (Type)
import Data.Type.Bool (type (&&))


class IsEnum (f :: Type -> Type) (r :: Bool) | f -> r

instance (IsEnum f lhs, IsEnum g rhs, r ~ (lhs && rhs)) => IsEnum (f :+: g) r
instance IsEnum f r => IsEnum (M1 i c f) r
instance IsEnum (f :*: g) 'False
instance IsEnum (f :.: g) 'False
instance IsEnum (K1 i c) 'False
instance IsEnum Par1 'False
instance IsEnum (Rec1 f) 'False
instance IsEnum U1 'True
