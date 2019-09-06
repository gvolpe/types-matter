{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}
{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses #-}

module Http where

import           Data.Text                      ( Text
                                                , null
                                                , pack
                                                , unpack
                                                )
import           Language.Haskell.TH.Syntax     ( Lift(..) )
import           Prelude                 hiding ( null )
import           Refined

instance Predicate NonEmpty Text where
  validate p value = validate p (unpack value)

newtype HttpHost = HttpHost { unHost :: Text } deriving Show
newtype HttpPort = HttpPort { unPort :: Int } deriving Show
newtype HttpUri = HttpUri { unUri :: Text } deriving Show

mkHttpHost :: Text -> Maybe HttpHost
mkHttpHost (null -> True) = Nothing
mkHttpHost h              = Just (HttpHost h)

-- Validation happens at runtime
mkHttpPort :: Int -> Maybe HttpPort
mkHttpPort n = if n >= 1024 && n <= 49151 then Just (HttpPort n) else Nothing

-- We assume the inputs are validated
mkUri :: HttpHost -> HttpPort -> HttpUri
mkUri HttpHost {..} HttpPort {..} =
  HttpUri (unHost <> ":" <> pack (show unPort))

-- Refinement types --

-- Validation happens at compile time
type HttpHost' = Refined NonEmpty Text
-- https://en.wikipedia.org/wiki/List_of_TCP_and_UDP_port_numbers
type HttpPort' = Refined (FromTo 1024 49151) Int
-- Refined (And (Not (LessThan 1024)) (Not (GreaterThan 49151))) Int

mkUri' :: HttpHost' -> HttpPort' -> HttpUri
mkUri' host port =
  HttpUri (unrefine host <> ":" <> pack (show $ unrefine port))

-- Or use: https://hackage.haskell.org/package/th-lift-instances
instance Lift Text where
  lift t = lift (unpack t)
