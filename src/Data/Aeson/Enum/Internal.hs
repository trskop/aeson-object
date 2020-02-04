{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
-- |
-- Module:      Data.Aeson.Enum.Internal
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- TODO: Module description.
module Data.Aeson.Enum.Internal
    ( EnumValue(..)
    , EnumEncoding(..)

    , enumValueToEncoding

    , ToJsonEnum(..)
    , toJson
    , toEncoding
    )
  where

import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson
    ( ToJSON(toJSON, toEncoding)
    , Value(String)
    )
import qualified Data.Aeson.Encoding as Aeson
    ( Encoding
    , Encoding'
    , text
    )
import qualified Data.Aeson.Encoding.Internal as Aeson (retagEncoding)
import Data.Text (Text)


newtype EnumValue = EnumValue {unpackEnumValue :: Text}
  deriving stock (Generic)

instance Aeson.ToJSON EnumValue where
    toJSON = toJson
    toEncoding = toEncoding

newtype EnumEncoding = EnumEncoding
    { unpackEnumEncoding :: Aeson.Encoding' Text
    }
  deriving stock (Generic)

enumValueToEncoding :: EnumValue -> EnumEncoding
enumValueToEncoding = \(EnumValue t) -> EnumEncoding (Aeson.text t)

-- {{{ ToJsonEnum -------------------------------------------------------------

class ToJsonEnum a where
    toJsonEnum :: a -> EnumValue

    toJsonEnumEncoding :: a -> EnumEncoding
    toJsonEnumEncoding = enumValueToEncoding . toJsonEnum
    {-# INLINE toJsonEnumEncoding #-}

toJson :: ToJsonEnum a => a -> Aeson.Value
toJson = Aeson.String . unpackEnumValue . toJsonEnum

toEncoding :: ToJsonEnum a => a -> Aeson.Encoding
toEncoding = Aeson.retagEncoding . unpackEnumEncoding . toJsonEnumEncoding

instance ToJsonEnum EnumValue where
    toJsonEnum :: EnumValue -> EnumValue
    toJsonEnum = id
    {-# INLINE toJsonEnum #-}

    toJsonEnumEncoding :: EnumValue -> EnumEncoding
    toJsonEnumEncoding = enumValueToEncoding
    {-# INLINE toJsonEnumEncoding #-}

-- }}} ToJsonEnum -------------------------------------------------------------
