-- |
-- Module:      Data.Aeson.Enum
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- TODO: Module description.
module Data.Aeson.Enum
    ( EnumValue
    , EnumEncoding
    , ToJsonEnum(..)
    , toJson
    , toEncoding
    , enumValueToEncoding
    )
  where

import Data.Aeson.Enum.Internal
    ( EnumValue
    , EnumEncoding
    , ToJsonEnum(..)
    , enumValueToEncoding
    , toEncoding
    , toJson
    )
