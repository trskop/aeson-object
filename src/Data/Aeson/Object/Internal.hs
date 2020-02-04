{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
-- |
-- Module:      Data.Aeson.Object.Internal
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- TODO: Module description.
module Data.Aeson.Object.Internal
    ( Object(..)
    , object

    , ObjectEncoding(..)
    , pairs

    , objectToEncoding

    , ToJsonObject(..)
    , toJson
    , toEncoding
    )
  where

import Data.Kind (Constraint, Type)

import qualified Data.Aeson as Aeson
    ( Object
    , ToJSON(toJSON, toEncoding)
    , Value(Object)
    , KeyValue
    )
import qualified Data.Aeson.Encoding as Aeson
    ( Encoding
    , Series
    , pair
    , pairs
    )
import qualified Data.Aeson.Types as Aeson (Pair)
import qualified Data.HashMap.Strict as HashMap (foldrWithKey, fromList)


newtype Object = Object {unpackObject :: Aeson.Object}
  deriving newtype (Aeson.KeyValue)

instance Aeson.ToJSON Object where
    toJSON = toJson
    {-# INLINE toJSON #-}

    toEncoding = toEncoding
    {-# INLINE toEncoding #-}

concatObjects :: Foldable t => t Object -> Object
concatObjects = Object . foldMap unpackObject
{-# INLINE concatObjects #-}

object :: [Aeson.Pair] -> Object
object = Object . HashMap.fromList
{-# INLINE object #-}

newtype ObjectEncoding = ObjectEncoding {unpackObjectEncoding :: Aeson.Series}
  deriving newtype (Aeson.KeyValue)

concatObjectEncodings :: Foldable t => t ObjectEncoding -> ObjectEncoding
concatObjectEncodings = ObjectEncoding . foldMap unpackObjectEncoding
{-# INLINE concatObjectEncodings #-}

pairs :: Aeson.Series -> ObjectEncoding
pairs = ObjectEncoding
{-# INLINE pairs #-}

objectToEncoding :: Object -> ObjectEncoding
objectToEncoding = \(Object o) ->
    ObjectEncoding (HashMap.foldrWithKey go mempty o)
  where
    go k v m = Aeson.pair k (Aeson.toEncoding v) <> m
{-# INLINE objectToEncoding #-}

-- {{{ ToJsonObject -----------------------------------------------------------

class ToJsonObject a where
    toJsonObject :: a -> Object

    toJsonObjectEncoding :: a -> ObjectEncoding
    toJsonObjectEncoding = objectToEncoding . toJsonObject
    {-# INLINE toJsonObjectEncoding #-}

toJson :: ToJsonObject a => a -> Aeson.Value
toJson = Aeson.Object . unpackObject . toJsonObject
{-# INLINE toJson #-}

toEncoding :: ToJsonObject a => a -> Aeson.Encoding
toEncoding = Aeson.pairs . unpackObjectEncoding . toJsonObjectEncoding
{-# INLINE toEncoding #-}

instance ToJsonObject Object where
    toJsonObject = id
    {-# INLINE toJsonObject #-}

    toJsonObjectEncoding = objectToEncoding
    {-# INLINE toJsonObjectEncoding #-}

-- TODO: Implement
type UniqueFields (ts :: [Type]) = (() :: Constraint)

instance
    ( UniqueFields [a, b]
    , ToJsonObject a
    , ToJsonObject b
    ) => ToJsonObject (a, b)
  where
    toJsonObject (a, b) =
        concatObjects [toJsonObject a, toJsonObject b]
    {-# INLINE toJsonObject #-}

    toJsonObjectEncoding (a, b) =
        concatObjectEncodings [toJsonObjectEncoding a, toJsonObjectEncoding b]
    {-# INLINE toJsonObjectEncoding #-}

instance
    ( UniqueFields [a, b, c]
    , ToJsonObject a
    , ToJsonObject b
    , ToJsonObject c
    ) => ToJsonObject (a, b, c)
  where
    toJsonObject (a, b, c) = concatObjects
        [ toJsonObject a
        , toJsonObject b
        , toJsonObject c
        ]
    {-# INLINE toJsonObject #-}

    toJsonObjectEncoding (a, b, c) = concatObjectEncodings
        [ toJsonObjectEncoding a
        , toJsonObjectEncoding b
        , toJsonObjectEncoding c
        ]
    {-# INLINE toJsonObjectEncoding #-}

instance
    ( UniqueFields [a1, a2, a3, a4]
    , ToJsonObject a1
    , ToJsonObject a2
    , ToJsonObject a3
    , ToJsonObject a4
    ) => ToJsonObject (a1, a2, a3, a4)
  where
    toJsonObject (a1, a2, a3, a4) = concatObjects
        [ toJsonObject a1
        , toJsonObject a2
        , toJsonObject a3
        , toJsonObject a4
        ]
    {-# INLINE toJsonObject #-}

    toJsonObjectEncoding (a1, a2, a3, a4) = concatObjectEncodings
        [ toJsonObjectEncoding a1
        , toJsonObjectEncoding a2
        , toJsonObjectEncoding a3
        , toJsonObjectEncoding a4
        ]
    {-# INLINE toJsonObjectEncoding #-}

instance
    ( UniqueFields [a1, a2, a3, a4, a5]
    , ToJsonObject a1
    , ToJsonObject a2
    , ToJsonObject a3
    , ToJsonObject a4
    , ToJsonObject a5
    ) => ToJsonObject (a1, a2, a3, a4, a5)
  where
    toJsonObject (a1, a2, a3, a4, a5) = concatObjects
        [ toJsonObject a1
        , toJsonObject a2
        , toJsonObject a3
        , toJsonObject a4
        , toJsonObject a5
        ]
    {-# INLINE toJsonObject #-}

    toJsonObjectEncoding (a1, a2, a3, a4, a5) = concatObjectEncodings
        [ toJsonObjectEncoding a1
        , toJsonObjectEncoding a2
        , toJsonObjectEncoding a3
        , toJsonObjectEncoding a4
        , toJsonObjectEncoding a5
        ]
    {-# INLINE toJsonObjectEncoding #-}

instance
    ( UniqueFields [a1, a2, a3, a4, a5, a6]
    , ToJsonObject a1
    , ToJsonObject a2
    , ToJsonObject a3
    , ToJsonObject a4
    , ToJsonObject a5
    , ToJsonObject a6
    ) => ToJsonObject (a1, a2, a3, a4, a5, a6)
  where
    toJsonObject (a1, a2, a3, a4, a5, a6) = concatObjects
        [ toJsonObject a1
        , toJsonObject a2
        , toJsonObject a3
        , toJsonObject a4
        , toJsonObject a5
        , toJsonObject a6
        ]
    {-# INLINE toJsonObject #-}

    toJsonObjectEncoding (a1, a2, a3, a4, a5, a6) = concatObjectEncodings
        [ toJsonObjectEncoding a1
        , toJsonObjectEncoding a2
        , toJsonObjectEncoding a3
        , toJsonObjectEncoding a4
        , toJsonObjectEncoding a5
        , toJsonObjectEncoding a6
        ]
    {-# INLINE toJsonObjectEncoding #-}

-- }}} ToJsonObject -----------------------------------------------------------
