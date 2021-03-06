-- |
-- Module:      Data.Aeson.Object
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- TODO: Module description.
module Data.Aeson.Object
    ( Object
    , ObjectEncoding
    , ToJsonObject(..)
    , toJson
    , toEncoding
    , object
    , pairs
    , objectToEncoding
    )
  where

import Data.Aeson.Object.Internal
    ( Object
    , ObjectEncoding
    , ToJsonObject(..)
    , object
    , objectToEncoding
    , pairs
    , toEncoding
    , toJson
    )
